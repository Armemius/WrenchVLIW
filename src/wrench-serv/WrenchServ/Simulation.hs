module WrenchServ.Simulation (
    SimulationRequest (..),
    SimulationResult (..),
    SimulationTask (..),
    TestCaseEntry (..),
    doSimulation,
    spitDump,
    spitSimulationRequest,
    dumpFn,
    statsFn,
    execLogFn,
    testCaseStatsFn,
    testCaseEntriesFn,
) where

import Control.Exception (catch)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, openTempFile)
import System.FilePath (takeDirectory)
import System.Process (readProcessWithExitCode)
import Web.FormUrlEncoded (FromForm)
import Wrench.Misc (wrenchVersion)
import WrenchServ.Config

data SimulationRequest = SimulationRequest
    { name :: Text
    , asm :: Text
    , config :: Text
    , comment :: Text
    , variant :: Maybe Text
    , isa :: Text
    }
    deriving (FromForm, Generic, Show)

nameFn
    , commentFn
    , variantFn
    , isaFn
    , configFn
    , asmFn
    , wrenchVersionFn
    , dumpFn
    , statsFn
    , testCaseStatsFn ::
        FilePath -> UUID -> FilePath
nameFn path guid = path <> "/" <> show guid <> "/name.txt"
commentFn path guid = path <> "/" <> show guid <> "/comment.txt"
variantFn path guid = path <> "/" <> show guid <> "/variant.txt"
isaFn path guid = path <> "/" <> show guid <> "/isa.txt"
configFn path guid = path <> "/" <> show guid <> "/config.yaml"
asmFn path guid = path <> "/" <> show guid <> "/source.s"
wrenchVersionFn path guid = path <> "/" <> show guid <> "/wrench-version.txt"
dumpFn path guid = path <> "/" <> show guid <> "/dump.txt"
statsFn path guid = path <> "/" <> show guid <> "/stats.log"
execLogFn :: FilePath -> UUID -> FilePath
execLogFn path guid = path <> "/" <> show guid <> "/exec_log.json"
testCaseStatsFn path guid = path <> "/" <> show guid <> "/test_cases_stats.log"

testCaseEntriesFn :: FilePath -> UUID -> FilePath
testCaseEntriesFn path guid = path <> "/" <> show guid <> "/test_cases.json"

spitSimulationRequest :: FilePath -> UUID -> SimulationRequest -> IO ()
spitSimulationRequest cStoragePath guid SimulationRequest{name, asm, config, comment, variant, isa} = do
    let dir = cStoragePath <> "/" <> show guid
    createDirectoryIfMissing True dir
    mapM_
        (\(mkFn, content) -> writeFileText (mkFn cStoragePath guid) content)
        [ (asmFn, asm)
        , (configFn, config)
        , (nameFn, name)
        , (commentFn, comment)
        , (variantFn, fromMaybe "-" variant)
        , (isaFn, isa)
        , (wrenchVersionFn, wrenchVersion)
        ]

data SimulationTask = SimulationTask
    { stIsa :: Text
    , stAsmFn :: FilePath
    , stConfFn :: FilePath
    , stGuid :: UUID
    }
    deriving (FromForm, Generic, Show)

data SimulationResult = SimulationResult
    { srExitCode :: ExitCode
    , srOutput :: Text
    , srError :: Text
    , srCmd :: Text
    , srStatusLog :: Text
    , srTestCaseStatus :: Text
    , srTestCase :: Text
    , srSuccess :: Bool
    , srStats :: Maybe Text
    , srConfigPath :: FilePath
    }
    deriving (Generic, Show)

data TestCaseEntry = TestCaseEntry
    { tceName :: Text
    , tceStatus :: Text
    , tceSuccess :: Bool
    , tceStats :: Maybe Text
    , tceLog :: Text
    , tceExitCode :: Int
    }
    deriving (Generic, Show)

instance ToJSON TestCaseEntry

instance FromJSON TestCaseEntry

spitDump :: Config -> SimulationTask -> IO ()
spitDump Config{cStoragePath, cWrenchPath, cWrenchArgs} SimulationTask{stIsa, stAsmFn, stGuid, stConfFn} = do
    let args = cWrenchArgs <> ["--isa", toString stIsa, stAsmFn, "-c", stConfFn, "-S"]
    (_exitCode, stdoutDump, stderrDump) <- readProcessWithExitCode cWrenchPath args ""
    writeFileText (dumpFn cStoragePath stGuid) $ unlines $ map toText [stdoutDump, stderrDump]

doSimulation :: Config -> SimulationTask -> IO SimulationResult
doSimulation Config{cWrenchPath, cWrenchArgs, cLogLimit, cStoragePath} SimulationTask{stIsa, stAsmFn, stConfFn, stGuid} = do
    (tmpStatsPath, tmpHandle) <- openTempFile "/tmp" "wrench-stats.log"
    hClose tmpHandle
    (tmpExecLogPath, tmpExecHandle) <- openTempFile "/tmp" "wrench-exec-log.json"
    hClose tmpExecHandle
    let args =
            cWrenchArgs
                <> ["--isa", toString stIsa, stAsmFn, "-c", stConfFn, "--stats-file", tmpStatsPath, "--exec-log", tmpExecLogPath]
        srCmd = T.intercalate " " $ map toText ([cWrenchPath] <> args)
    simConf <- decodeUtf8 <$> readFileBS stConfFn
    currentTime <- getCurrentTime
    (srExitCode, out, err) <- readProcessWithExitCode cWrenchPath args ""
    statsText <- do
        exist <- doesFileExist tmpStatsPath
        if exist
            then Just . decodeUtf8 <$> readFileBS tmpStatsPath
            else return Nothing
    execLogPayload <- do
        exist <- doesFileExist tmpExecLogPath
        if exist
            then Just <$> readFileBS tmpExecLogPath
            else return Nothing
    removeFile tmpStatsPath `catch` \(_ :: SomeException) -> return ()
    removeFile tmpExecLogPath `catch` \(_ :: SomeException) -> return ()
    for_ execLogPayload $ \payload -> do
        let dest = execLogFn cStoragePath stGuid
        createDirectoryIfMissing True (takeDirectory dest)
        writeFileBS dest payload
    let srStatusLog =
            T.intercalate
                "\n"
                ["$ date", show currentTime, "$ wrench --version", wrenchVersion, srCmd, show srExitCode, toText srError]
        stdoutText = toText out
        srOutput =
            if T.length stdoutText > cLogLimit
                then "LOG TOO LONG, CROPPED\n\n" <> T.drop (T.length stdoutText - cLogLimit) stdoutText
                else toText stdoutText
        srError = toText err
        srTestCaseStatus = toText stConfFn <> ": " <> show srExitCode <> "\n" <> srError
        srTestCase =
            T.intercalate
                "\n\n"
                [ "# " <> toText stConfFn
                , simConf <> "==="
                , srOutput <> srError
                , "==="
                , srCmd
                ]
    return
        $ SimulationResult
            { srExitCode
            , srSuccess = srExitCode == ExitSuccess
            , srOutput
            , srError
            , srCmd
            , srStatusLog
            , srTestCase
            , srTestCaseStatus
            , srStats = statsText
            , srConfigPath = stConfFn
            }
