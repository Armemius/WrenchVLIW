{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Wrench.Debug.Execution.Test (tests) where

import Relude
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Wrench.Config (Config (..))
import Wrench.Debug.Execution (ExecState (..), ExecutionLog (..), StepEntry (..))
import Wrench.Isa.RiscIv (RiscIvState)
import Wrench.Isa.RiscIv qualified as RiscIv
import Wrench.Machine.Types (Cell)
import Wrench.Wrench (Options (..), Result (..), wrench)

type RiscMem = IntMap (Cell (RiscIv.Isa Int32 Int32) Int32)

runResult :: Config -> Text -> IO (Result RiscMem Int32)
runResult conf src =
  case wrench @(RiscIvState Int32) opts conf (toString src) of
    Left err -> assertFailure ("wrench failed: " <> T.unpack err)
    Right res -> return res
  where
    opts =
      def
        { input = "test.s"
        , isa = "risc-iv-32"
        , onlyTranslation = False
        , verbose = False
        }

runExecLog :: Config -> Text -> IO ExecutionLog
runExecLog conf src = do
  Result {rExecution} <- runResult conf src
  case rExecution of
    Nothing -> assertFailure "missing execution log"
    Just log -> return log

isHaltInstruction :: StepEntry -> Bool
isHaltInstruction StepEntry {seInstruction} =
  maybe False (T.isInfixOf "halt" . T.toLower) seInstruction

haltProgram :: Text
haltProgram =
  T.unlines
    [ "    .text"
    , ""
    , "_start:"
    , "    halt"
    ]

limitProgram :: Text
limitProgram =
  T.unlines
    [ "    .text"
    , ""
    , "_start:"
    , "    addi t0, t0, 1"
    , "    halt"
    ]

dataProgram :: Text
dataProgram =
  T.unlines
    [ "    .data"
    , "buf:             .byte  0x00, 0x2A"
    , "    .text"
    , ""
    , "_start:"
    , "    halt"
    ]

tests :: TestTree
tests =
  testGroup
    "Debug Execution"
    [ testCase "includes halt step" $ do
        let conf = def {cLimit = 10, cMemorySize = 16, cReports = Nothing}
        log <- runExecLog conf haltProgram
        length (elSteps log) @?= 1
        case viaNonEmpty last (elSteps log) of
          Nothing -> assertFailure "missing last step"
          Just lastStep ->
            assertBool "halt step missing" (isHaltInstruction lastStep)
    , testCase "does not append halt on limit" $ do
        let conf = def {cLimit = 1, cMemorySize = 16, cReports = Nothing}
        log <- runExecLog conf limitProgram
        assertBool "unexpected steps" (null (elSteps log))
    , testCase "memory ranges and sparse values" $ do
        let conf = def {cLimit = 10, cMemorySize = 16, cReports = Nothing}
        Result {rExecution, rLabels} <- runResult conf dataProgram
        log <- case rExecution of
          Nothing -> assertFailure "missing execution log"
          Just v -> return v
        let ExecState {esMemorySize, esMemoryProgram, esMemory} = elInitial log
        esMemorySize @?= 16
        assertBool "zero bytes should be omitted" (0 `notElem` HM.elems esMemory)
        assertBool "missing non-zero byte" (42 `elem` HM.elems esMemory)
        case HM.lookup "_start" rLabels of
          Nothing -> assertFailure "missing _start label"
          Just start ->
            assertBool
              "program range missing _start"
              ((fromEnum start, fromEnum start + 3) `elem` esMemoryProgram)
    ]
