{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Crypto.Hash.SHA1 qualified as SHA1
import Data.Aeson (decodeStrict, encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text (isSuffixOf, replace)
import Data.Text qualified as T
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Lucid (Html, renderText, toHtml, toHtmlRaw)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Numeric (showHex)
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeBaseName, takeFileName, (</>))
import Wrench.Misc (wrenchVersion)
import WrenchServ.Config
import WrenchServ.Simulation
import WrenchServ.Statistics

formatCodeWithLineNumbers :: Text -> Text
formatCodeWithLineNumbers code =
    let codeLines = T.lines code
        lineCount = length codeLines
        lineNumbers = T.concat $ map (\i -> "<div class=\"line-number\">" <> show i <> "</div>") [1 .. lineCount]
        codeContent = T.concat $ map (\line -> "<div class=\"code-line\">" <> escapeHtml line <> "</div>") codeLines
        container =
            "<div class=\"code-container\"><div class=\"line-numbers\">"
                <> lineNumbers
                <> "</div><div class=\"code-content\">"
                <> codeContent
                <> "</div></div>"
     in container

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    conf@Config{cPort} <- initConfig
    print $ mask conf
    putStrLn $ "Starting server on port " <> show cPort
    run cPort (logStdoutDev $ app conf)

app :: Config -> Application
app conf = serve (Proxy :: Proxy API) (server conf)

type API =
    "submit-form" :> GetForm
        :<|> "submit" :> SubmitForm
        :<|> "report" :> GetReport
        :<|> "assets" :> Raw
        :<|> Get '[JSON] (Headers '[Header "Location" Text] NoContent)

server :: Config -> Server API
server conf =
    getForm conf
        :<|> submitForm conf
        :<|> getReport conf
        :<|> serveDirectoryWebApp "static/assets"
        :<|> redirectToForm

type GetForm = Header "Cookie" Text :> Get '[HTML] (Html ())

getForm :: Config -> Maybe Text -> Handler (Html ())
getForm conf@Config{cVariants} cookie = do
    let options = map (\v -> "<option value=\"" <> toText v <> "\">" <> toText v <> "</option>") cVariants
    template <- liftIO (decodeUtf8 <$> readFileBS "static/form.html")
    let renderedTemplate =
            foldl'
                (\st (pat, new) -> replace pat new st)
                template
                [ ("{{variants}}", mconcat options)
                , ("{{version}}", wrenchVersion)
                , ("{{tracker}}", postHogTracker)
                ]
    liftIO $ do
        track <- getTrack cookie
        posthogId <- getPosthogIdFromCookie cookie (track <> "_mp")
        trackEvent
            conf
            GetFormEvent
                { mpVersion = wrenchVersion
                , mpTrack = track
                , mpPosthogId = posthogId
                }
    return $ toHtmlRaw renderedTemplate

type SubmitForm =
    Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] SimulationRequest
        :> Post '[JSON] (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)

now :: IO Int
now = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

submitForm ::
    Config
    -> Maybe Text
    -> SimulationRequest
    -> Handler (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
submitForm conf@Config{cStoragePath, cVariantsPath} cookie task@SimulationRequest{name, variant, isa, asm, config} = do
    startAt <- liftIO now
    guid <- liftIO nextRandom
    liftIO $ spitSimulationRequest cStoragePath guid task

    let dir = cStoragePath <> "/" <> show guid
        asmFile = dir <> "/source.s"
        configFile = dir <> "/config.yaml"

    let simulationTask = SimulationTask{stIsa = isa, stAsmFn = asmFile, stConfFn = configFile, stGuid = guid}

    liftIO $ spitDump conf simulationTask

    SimulationResult{srOutput, srStatusLog, srStats, srSuccess = userSimSuccess} <- liftIO $ doSimulation conf simulationTask

    liftIO $ writeFileText (dir <> "/status.log") srStatusLog
    liftIO $ writeFileText (dir <> "/result.log") srOutput
    liftIO $ writeFileText (statsFn cStoragePath guid) $ fromMaybe "" srStats

    varChecks <- case variant of
        Nothing -> return []
        Just variant' -> do
            yamlFiles <- liftIO $ listTextCases (cVariantsPath </> toString variant')
            liftIO $ forM yamlFiles $ \yamlFile -> do
                doSimulation conf simulationTask{stConfFn = cVariantsPath </> toString variant' </> yamlFile}

    liftIO $ writeFile (dir <> "/test_cases_status.log") ""
    forM_ varChecks $ \(SimulationResult{srTestCaseStatus}) -> do
        let tsStatus = dir <> "/test_cases_status.log"
        liftIO $ appendFileText tsStatus srTestCaseStatus

    liftIO $ writeFile (dir <> "/test_cases_result.log") ""
    liftIO $ writeFile (testCaseStatsFn cStoragePath guid) ""

    let wins = filter (\(SimulationResult{srExitCode}) -> srExitCode == ExitSuccess) varChecks
        fails = filter (\(SimulationResult{srExitCode}) -> srExitCode /= ExitSuccess) varChecks
    forM_ (take 1 fails) $ \(SimulationResult{srTestCase}) -> do
        let testCaseLogFn = dir <> "/test_cases_result.log"
        liftIO $ writeFileText testCaseLogFn srTestCase

    forM_ varChecks $ \(SimulationResult{srStats = srStatsCase, srConfigPath}) -> do
        let testCaseStatsFn' = testCaseStatsFn cStoragePath guid
        let header = "# " <> toText srConfigPath <> "\n"
        liftIO $ appendFileText testCaseStatsFn' $ header <> fromMaybe "stats not available\n" srStatsCase <> "\n"

    -- Persist structured test case data for the report view.
    let testCaseEntries =
            map
                ( \SimulationResult{srConfigPath, srTestCaseStatus, srSuccess, srStats = srStatsCase, srTestCase, srExitCode} ->
                    TestCaseEntry
                        { tceName = toText srConfigPath
                        , tceStatus = srTestCaseStatus
                        , tceSuccess = srSuccess
                        , tceStats = srStatsCase
                        , tceLog = srTestCase
                        , tceExitCode = case srExitCode of
                            ExitSuccess -> 0
                            ExitFailure n -> n
                        }
                )
                varChecks
        testCaseEntriesPath = testCaseEntriesFn cStoragePath guid
    liftIO $ writeFileBS testCaseEntriesPath (BL.toStrict $ encode testCaseEntries)

    endAt <- liftIO now
    track <- liftIO $ getTrack cookie
    posthogId <- liftIO $ getPosthogIdFromCookie cookie (track <> "_mp")
    let event =
            SimulationEvent
                { mpGuid = guid
                , mpName = name
                , mpIsa = isa
                , mpVariant = variant
                , mpVersion = wrenchVersion
                , mpTrack = track
                , mpAsmSha1 = sha1 asm
                , mpYamlSha1 = sha1 config
                , mpWinCount = length wins
                , mpFailCount = length fails
                , mpPosthogId = posthogId
                , mpSuccess = userSimSuccess
                , mpDuration = toEnum $ endAt - startAt
                , mpVariantSuccess =
                    if not $ null varChecks
                        then Just $ null fails
                        else Nothing
                }
    liftIO $ trackEvent conf event
    let locationHeader = ("Location", "/report/" <> show guid)
        cookieHeader = ("Set-Cookie", encodeUtf8 $ trackCookie track)
    throwError
        $ err301{errHeaders = [locationHeader, cookieHeader]}

type GetReport =
    Header "Cookie" Text
        :> Capture "guid" UUID
        :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))

getReport :: Config -> Maybe Text -> UUID -> Handler (Headers '[Header "Set-Cookie" Text] (Html ()))
getReport conf@Config{cStoragePath} cookie guid = do
    let dir = cStoragePath <> "/" <> show guid

    nameContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/name.txt"))
    variantContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/variant.txt"))
    commentContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/comment.txt"))
    asmContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/source.s"))
    configContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/config.yaml"))
    logContent <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/result.log"))
    status <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/status.log"))
    stats <- liftIO (fromMaybe "" <$> maybeReadFile (statsFn cStoragePath guid))
    testCaseStatus <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_status.log"))
    testCaseResult <- liftIO (decodeUtf8 <$> readFileBS (dir <> "/test_cases_result.log"))
    testCaseStats <- liftIO (fromMaybe "" <$> maybeReadFile (testCaseStatsFn cStoragePath guid))
    testCaseEntries <- liftIO $ parseTestCases (testCaseEntriesFn cStoragePath guid)
    reportWrenchVersion <- liftIO $ do
        exist <- doesFileExist (dir <> "/wrench-version.txt")
        if exist
            then decodeUtf8 <$> readFileBS (dir <> "/wrench-version.txt")
            else return "< 0.2.11"
    dump <- liftIO (fromMaybe "DUMP NOT AVAILABLE" <$> maybeReadFile (dumpFn cStoragePath guid))
    let versionWarning =
            if reportWrenchVersion == wrenchVersion
                then ""
                else "Warning: report generated with wrench " <> reportWrenchVersion <> " but server is " <> wrenchVersion <> "."

    template <- liftIO (decodeUtf8 <$> readFileBS "static/result.html")

    let templateWithBasicContent =
            foldl'
                (\st (pat, new) -> replace pat new st)
                (replace "{{tracker}}" postHogTracker template)
                [ ("{{name}}", escapeHtml nameContent)
                , ("{{variant}}", escapeHtml variantContent)
                , ("{{comment}}", escapeHtml commentContent)
                , ("{{status}}", escapeHtml status)
                , ("{{stats}}", escapeHtml stats)
                , ("{{test_cases_status}}", escapeHtml testCaseStatus)
                , ("{{test_cases_cards}}", testCaseCards testCaseEntries)
                , ("{{version_warning}}", escapeHtml versionWarning)
                ]

    let renderTemplate =
            foldl'
                (\st (pat, new) -> replace pat new st)
                templateWithBasicContent
                [ ("{{assembler_code}}", formatCodeWithLineNumbers asmContent)
                , ("{{yaml_content}}", formatCodeWithLineNumbers configContent)
                , ("{{result}}", formatCodeWithLineNumbers logContent)
                , ("{{test_cases_result}}", formatCodeWithLineNumbers testCaseResult)
                , ("{{test_cases_stats}}", formatCodeWithLineNumbers testCaseStats)
                , ("{{test_cases_cards}}", testCaseCards testCaseEntries)
                , ("{{dump}}", formatCodeWithLineNumbers dump)
                ]

    track <- liftIO $ getTrack cookie
    posthogId <- liftIO $ getPosthogIdFromCookie cookie (track <> "_mp")
    let event =
            ReportViewEvent
                { mpGuid = guid
                , mpName = nameContent
                , mpVersion = wrenchVersion
                , mpTrack = track
                , mpPosthogId = posthogId
                , mpWrenchVersion = reportWrenchVersion
                }
    liftIO $ trackEvent conf event
    return $ addHeader (trackCookie track) $ toHtmlRaw renderTemplate

redirectToForm :: Handler (Headers '[Header "Location" Text] NoContent)
redirectToForm = throwError $ err301{errHeaders = [("Location", "/submit-form")]}

sortFiles :: [FilePath] -> [FilePath]
sortFiles = sortBy compareFiles
    where
        compareFiles a b =
            let nameA = takeBaseName a
                nameB = takeBaseName b
             in case (reads nameA :: [(Int, String)], reads nameB :: [(Int, String)]) of
                    ([(nA, "")], [(nB, "")]) -> compare nA nB
                    ([(_, "")], _) -> LT
                    (_, [(_, "")]) -> GT
                    _ -> compare nameA nameB

listTextCases :: FilePath -> IO [FilePath]
listTextCases path = do
    contents <- listDirectory path
    files <- filterM doesFileExist (map (path </>) contents)
    return $ sortFiles $ filter (isSuffixOf ".yaml" . toText) $ map takeFileName files

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile path = do
    doesFileExist path >>= \case
        True -> Just . decodeUtf8 <$> readFileBS path
        False -> return Nothing

escapeHtml :: Text -> Text
escapeHtml = toText . renderText . toHtml

parseTestCases :: FilePath -> IO [TestCaseEntry]
parseTestCases path = do
    exists <- doesFileExist path
    if not exists
        then return []
        else do
            bytes <- readFileBS path
            return $ fromMaybe [] (decodeStrict bytes)

statusColor :: TestCaseEntry -> Text
statusColor TestCaseEntry{tceExitCode, tceSuccess}
    | tceExitCode == 0 && tceSuccess = "border border-green-500 text-green-500 dark:text-green-500"
    | tceExitCode == 2 = "border border-yellow-500 text-yellow-500 dark:text-yellow-500"
    | otherwise = "border border-red-500 text-red-500 dark:text-red-500"

testCaseCards :: [TestCaseEntry] -> Text
testCaseCards [] = "No test cases."
testCaseCards entries =
    T.intercalate
        "\n"
        ( zipWith
            ( \idx TestCaseEntry{tceName, tceStatus, tceSuccess, tceStats, tceLog, tceExitCode} ->
                let target = "testcase-" <> show idx
                    colorClass = statusColor TestCaseEntry{tceName, tceStatus, tceSuccess, tceStats, tceLog, tceExitCode}
                    badgeText =
                        case tceExitCode of
                            0 | tceSuccess -> "passed"
                            2 -> "error"
                            _ -> "failed"
                    statsBlock =
                        maybe
                            ""
                            (\s -> "<h4 class=\"mt-2 text-[var(--c-grey)]\">/* stats */</h4><pre class=\"bg-[var(--c-dark-grey)] p-3 rounded\">" <> escapeHtml s <> "</pre>")
                            tceStats
                 in mconcat
                        [ "<details class=\"bg-[var(--c-dark-grey)] mb-2 rounded-lg overflow-hidden\" id=\""
                        , target
                        , "\">"
                        , "<summary class=\"flex justify-between items-center cursor-pointer px-4 py-2 hover:bg-[var(--c-grey)] hover:text-[var(--c-black)]\">"
                        , "<span class=\"font-mono\">" <> escapeHtml tceName <> "</span>"
                        , "<span class=\"px-2 py-1 ml-2 rounded "
                        , colorClass
                        , "\">"
                        , badgeText
                        , "</span>"
                        , "</summary>"
                        , "<div class=\"p-4 bg-[var(--c-black)]\">"
                        , "<h4 class=\"text-[var(--c-grey)]\">/* status */</h4>"
                        , "<pre class=\"bg-[var(--c-dark-grey)] p-3 rounded\">" <> escapeHtml tceStatus <> "</pre>"
                        , statsBlock
                        , "<h4 class=\"mt-2 text-[var(--c-grey)]\">/* report */</h4>"
                        , "<pre class=\"bg-[var(--c-dark-grey)] p-3 rounded overflow-x-auto\">"
                        , escapeHtml tceLog
                        , "</pre>"
                        , "</div>"
                        , "</details>"
                        ]
            )
            [(1 :: Int) ..]
            entries
        )

sha1 :: Text -> Text
sha1 text =
    let noSpaceText = T.replace " " "" $ T.replace "\n" "" $ T.replace "\t" " " text
        ctx0 = SHA1.init
        ctx = SHA1.update ctx0 $ encodeUtf8 noSpaceText
     in toText $ B.foldr showHex "" $ SHA1.hash $ SHA1.finalize ctx
