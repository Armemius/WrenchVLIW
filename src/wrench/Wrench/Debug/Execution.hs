module Wrench.Debug.Execution (
    ExecutionLog (..),
    ExecState (..),
    StepEntry (..),
    buildExecutionLog,
    writeExecutionLog,
) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BL
import Data.HashSet qualified as HS
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Relude
import Relude.Extra (toPairs, (!?))
import Wrench.Machine.Memory (Memory (..))
import Wrench.Machine.Types
import Wrench.Translator.Types (SourceInfo (..))

data IoState = IoState
    { iosAddr :: !Int
    , iosInput :: ![Integer]
    , iosOutput :: ![Integer]
    }
    deriving (Generic, Show, ToJSON)

data ExecState = ExecState
    { esPc :: !Int
    , esRegisters :: !(HashMap Text Integer)
    , esStacks :: !(HashMap Text [Integer])
    , esMemorySize :: !Int
    , esMemoryProgram :: ![(Int, Int)]
    , esMemory :: !(HashMap Int Word8)
    , esIo :: ![IoState]
    }
    deriving (Generic, Show, ToJSON)

data RegChange = RegChange
    { rcName :: !Text
    , rcBefore :: !Integer
    , rcAfter :: !Integer
    }
    deriving (Generic, Show, ToJSON)

data StackChange = StackChange
    { scName :: !Text
    , scBefore :: ![Integer]
    , scAfter :: ![Integer]
    }
    deriving (Generic, Show, ToJSON)

data MemChange = MemChange
    { mcAddr :: !Int
    , mcBefore :: !(Maybe Word8)
    , mcAfter :: !(Maybe Word8)
    }
    deriving (Generic, Show, ToJSON)

data IoChange = IoChange
    { icAddr :: !Int
    , icConsumed :: ![Integer]
    , icProduced :: ![Integer]
    }
    deriving (Generic, Show, ToJSON)

data StepEntry = StepEntry
    { seIndex :: !Int
    , sePc :: !Int
    , seNextPc :: !(Maybe Int)
    , seLabel :: !(Maybe Text)
    , seInstruction :: !(Maybe Text)
    , seSource :: !(Maybe SourceInfo)
    , seRegisters :: ![RegChange]
    , seStacks :: ![StackChange]
    , seMemory :: ![MemChange]
    , seIo :: ![IoChange]
    }
    deriving (Generic, Show, ToJSON)

data ExecutionLog = ExecutionLog
    { elInitial :: !ExecState
    , elSteps :: ![StepEntry]
    }
    deriving (Generic, Show, ToJSON)

writeExecutionLog :: FilePath -> ExecutionLog -> IO ()
writeExecutionLog path = BL.writeFile path . encode

buildExecutionLog ::
    forall st m isa w.
    ( MachineWord w
    , Memory m isa w
    , Show isa
    , StateInterspector st m isa w
    ) =>
    HashMap String w
    -> HashMap Int SourceInfo
    -> [Trace st isa]
    -> Maybe ExecutionLog
buildExecutionLog labels sourceMap traces = do
    (initialState, _) <- uncons states
    let initialCells = dumpCells $ memoryDump initialState
        programRanges = collectProgramRanges initialCells
        initialSnapshot = snapshot programRanges initialState
        steps = zipWith (mkStep programRanges) [0 ..] (zip states (drop 1 states))
    return
        ExecutionLog
            { elInitial = initialSnapshot
            , elSteps = steps
            }
    where
        states = [st | TState st <- traces]
        pcLabels :: HashMap Int Text
        pcLabels = fromList $ map (\(l, a) -> (fromEnum a, toText l)) $ toPairs labels

        snapshot :: [(Int, Int)] -> st -> ExecState
        snapshot programRanges st =
            let cells = dumpCells $ memoryDump st
                memorySize = maybe 0 ((+ 1) . fst) (IntMap.lookupMax cells)
             in ExecState
                    { esPc = programCounter st
                    , esRegisters = fmap toMachineInt (stateRegisters st)
                    , esStacks = fmap (map toMachineInt) (stateStacks st)
                    , esMemorySize = memorySize
                    , esMemoryProgram = programRanges
                    , esMemory = collectValues cells
                    , esIo =
                        map
                            ( \(addr, (is, os)) ->
                                IoState
                                    { iosAddr = addr
                                    , iosInput = map toMachineInt is
                                    , iosOutput = map toMachineInt os
                                    }
                            )
                            (toPairs $ ioStreams st)
                    }

        mkStep :: [(Int, Int)] -> Int -> (st, st) -> StepEntry
        mkStep programRanges idx (cur, nxt) =
            let curSnap = snapshot programRanges cur
                nextSnap = snapshot programRanges nxt
                pc = esPc curSnap
                instructionText =
                    case readInstruction (memoryDump cur) pc of
                        Right instr -> Just $ T.pack $ show instr
                        Left _ -> Nothing
                regChanges = diffMap esRegisters curSnap nextSnap mkReg
                stackChanges = diffMap esStacks curSnap nextSnap mkStack
                memChanges = diffMemory (esMemory curSnap) (esMemory nextSnap)
                ioChanges = diffIo (ioMap curSnap) (ioMap nextSnap)
             in StepEntry
                    { seIndex = idx
                    , sePc = pc
                    , seLabel = pcLabels !? pc
                    , seInstruction = instructionText
                    , seSource = sourceMap !? pc
                    , seNextPc = Just (esPc nextSnap)
                    , seRegisters = regChanges
                    , seStacks = stackChanges
                    , seMemory = memChanges
                    , seIo = ioChanges
                    }
            where
                mkReg name before after = RegChange{rcName = name, rcBefore = before, rcAfter = after}
                mkStack name before after = StackChange{scName = name, scBefore = before, scAfter = after}
                ioMap ExecState{esIo} = fromList $ map (\IoState{iosAddr, iosInput, iosOutput} -> (iosAddr, (iosInput, iosOutput))) esIo

        diffMap ::
            (Eq a, Hashable k) =>
            (ExecState -> HashMap k a)
            -> ExecState
            -> ExecState
            -> (k -> a -> a -> b)
            -> [b]
        diffMap getter curSnap nextSnap ctor =
            let curMap = getter curSnap
                nextMap = getter nextSnap
                allKeys =
                    HS.toList
                        $ HS.fromList (map fst (toPairs curMap) <> map fst (toPairs nextMap))
             in mapMaybe
                    ( \k -> do
                        before <- curMap !? k
                        after <- nextMap !? k
                        guard (before /= after)
                        return $ ctor k before after
                    )
                    allKeys

        diffMemory :: HashMap Int Word8 -> HashMap Int Word8 -> [MemChange]
        diffMemory before after =
            let allKeys = ordNub $ map fst (toPairs before) <> map fst (toPairs after)
             in mapMaybe
                    ( \addr ->
                        let b = before !? addr
                            a = after !? addr
                         in if b /= a then Just MemChange{mcAddr = addr, mcBefore = b, mcAfter = a} else Nothing
                    )
                    allKeys

        diffIo :: HashMap Int ([Integer], [Integer]) -> HashMap Int ([Integer], [Integer]) -> [IoChange]
        diffIo before after =
            let allKeys = ordNub $ map fst (toPairs before) <> map fst (toPairs after)
             in mapMaybe
                    ( \addr -> do
                        let (inBefore, outBefore) = fromMaybe ([], []) (before !? addr)
                            (inAfter, outAfter) = fromMaybe ([], []) (after !? addr)
                            consumed =
                                let d = length inBefore - length inAfter
                                 in if d > 0 then take d inBefore else []
                            produced =
                                let d = length outAfter - length outBefore
                                 in if d > 0 then take d outAfter else []
                        if null consumed && null produced
                            then Nothing
                            else Just IoChange{icAddr = addr, icConsumed = consumed, icProduced = produced}
                    )
                    allKeys

        collectValues :: IntMap (Cell isa w) -> HashMap Int Word8
        collectValues =
            fromList
                . mapMaybe
                    ( \(addr, cell) ->
                        case cell of
                            Value v | v /= 0 -> Just (addr, v)
                            _ -> Nothing
                    )
                . toPairs

        collectProgramRanges :: IntMap (Cell isa w) -> [(Int, Int)]
        collectProgramRanges cells =
            let addrs =
                    [ addr
                    | (addr, cell) <- toPairs cells
                    , isProgramCell cell
                    ]
             in reverse $ foldl' step [] addrs
            where
                isProgramCell Instruction{} = True
                isProgramCell InstructionPart = True
                isProgramCell _ = False
                step [] addr = [(addr, addr)]
                step ((start, end) : rest) addr
                    | addr == end + 1 = (start, addr) : rest
                    | otherwise = (addr, addr) : (start, end) : rest

        toMachineInt :: (Enum a) => a -> Integer
        toMachineInt = toInteger . fromEnum
