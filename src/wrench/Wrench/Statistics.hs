module Wrench.Statistics (
    MemoryUsage (..),
    SlotUsage (..),
    StackUsage (..),
    SimulationStats (..),
    SimHook (..),
    CompileSlotHook (..),
    collectStats,
    formatStats,
) where

import Data.Text qualified as T
import Numeric (showFFloat)
import Relude
import Relude.Extra (maximum1, toPairs)
import Wrench.Isa.Acc32 qualified as Acc32
import Wrench.Isa.F32a qualified as F32a
import Wrench.Isa.M68k qualified as M68k
import Wrench.Isa.RiscIv qualified as RiscIv
import Wrench.Isa.VliwIv qualified as Vliw
import Wrench.Machine.Memory (Memory (..), readInstruction)
import Wrench.Machine.Types

data MemoryUsage = MemoryUsage
    { muSectionBytes :: Int
    , muContiguousBytes :: Int
    , muUsedBytes :: Int
    }
    deriving (Show)

data StackUsage = StackUsage
    { suDataMax :: Int
    , suReturnMax :: Int
    }
    deriving (Show)

data SlotUsage = SlotUsage
    { suTotalSlots :: Int
    , suNopSlots :: Int
    }
    deriving (Show)

data SimulationStats = SimulationStats
    { ssMemory :: MemoryUsage
    , ssInstructionCount :: Int
    , ssStackUsage :: Maybe StackUsage
    , ssSlotUsageRuntime :: Maybe SlotUsage
    , ssSlotUsageCompile :: Maybe SlotUsage
    }
    deriving (Show)

class SimHook st isa w | st -> isa w where
    stackHook :: [Trace st isa] -> Maybe StackUsage
    stackHook _ = Nothing

    slotHook :: [Trace st isa] -> Maybe SlotUsage
    slotHook _ = Nothing

instance (MachineWord w) => SimHook (Acc32.Acc32State w) (Acc32.Isa w w) w
instance (MachineWord w) => SimHook (RiscIv.RiscIvState w) (RiscIv.Isa w w) w
instance (MachineWord w) => SimHook (M68k.M68kState w) (M68k.Isa w w) w

instance (MachineWord w) => SimHook (F32a.F32aState w) (F32a.Isa w w) w where
    stackHook traces =
        let stacks = [F32a.stackDepths st | TState st <- traces]
         in case stacks of
                [] -> Nothing
                _ ->
                    let (!dataMax, !returnMax) = foldl' (\(d, r) (d', r') -> (max d d', max r r')) (0, 0) stacks
                     in Just StackUsage{suDataMax = dataMax, suReturnMax = returnMax}

instance (MachineWord w) => SimHook (Vliw.VliwIvState w) (Vliw.Isa w w) w where
    slotHook traces =
        let perInstr =
                mapMaybe
                    ( \case
                        TState st -> either (const Nothing) (Just . Vliw.slotNopCount) (readInstruction (memoryDump st) (programCounter st))
                        _ -> Nothing
                    )
                    traces
            (totalSlots, nopSlots) = foldl' (\(t, n) (t', n') -> (t + t', n + n')) (0, 0) perInstr
         in if totalSlots == 0 then Nothing else Just SlotUsage{suTotalSlots = totalSlots, suNopSlots = nopSlots}

collectStats ::
    (SimHook st isa w) =>
    [(Int, Int)] ->
    Maybe SlotUsage ->
    [Trace st isa] ->
    SimulationStats
collectStats sectionsInfo compileSlot traces =
    SimulationStats
        { ssMemory = memoryStats sectionsInfo
        , ssInstructionCount = length [() | TState{} <- traces]
        , ssStackUsage = stackHook traces
        , ssSlotUsageRuntime = slotHook traces
        , ssSlotUsageCompile = compileSlot
        }

class CompileSlotHook (isa :: Type -> Type -> Type) w where
    compileSlotUsage :: IntMap (Cell (isa w w) w) -> Maybe SlotUsage

instance CompileSlotHook isa w where
    compileSlotUsage _ = Nothing

instance CompileSlotHook Vliw.Isa w where
    compileSlotUsage cells =
        let instrs = mapMaybe (\case (_addr, Instruction i) -> Just i; _ -> Nothing) $ toPairs cells
            (totalSlots, nopSlots) = foldl' (\(t, n) i -> let (t', n') = Vliw.slotNopCount i in (t + t', n + n')) (0, 0) instrs
         in if totalSlots == 0 then Nothing else Just SlotUsage{suTotalSlots = totalSlots, suNopSlots = nopSlots}

memoryStats :: [(Int, Int)] -> MemoryUsage
memoryStats sectionsInfo =
    let sectionBytes = sum $ map snd sectionsInfo
        sorted = sortOn fst sectionsInfo
        contiguous = contiguousSize 0 sorted
        usedBytes = fromMaybe 0 $ viaNonEmpty maximum1 (map (uncurry (+)) sorted)
     in MemoryUsage{muSectionBytes = sectionBytes, muContiguousBytes = contiguous, muUsedBytes = usedBytes}

contiguousSize :: Int -> [(Int, Int)] -> Int
contiguousSize cur [] = cur
contiguousSize cur ((off, size) : rest)
    | off > cur = cur
    | otherwise = contiguousSize (max cur (off + size)) rest

formatStats :: SimulationStats -> Text
formatStats SimulationStats{ssMemory = MemoryUsage{muSectionBytes, muContiguousBytes, muUsedBytes}, ssInstructionCount, ssStackUsage, ssSlotUsageRuntime, ssSlotUsageCompile} =
    unlines
        $ filter
            (not . T.null)
            [ "=== Simulation info ==="
            , "Memory usage:"
            , "  Sections total: " <> showT muSectionBytes <> " bytes"
            , "  Dump until first gap: " <> showT muContiguousBytes <> " bytes"
            , "  Used address space: " <> showT muUsedBytes <> " bytes"
            , maybe "" (\StackUsage{suDataMax, suReturnMax} -> "  F32A stack depth (data/return): " <> showT suDataMax <> "/" <> showT suReturnMax) ssStackUsage
            , "Instruction execution:"
            , "  Executed instructions: " <> showT ssInstructionCount
            , maybe "" (formatSlot "runtime") ssSlotUsageRuntime
            , maybe "" (formatSlot "compile") ssSlotUsageCompile
            ]
    where
        formatSlot label SlotUsage{suTotalSlots, suNopSlots} =
            let percent = if suTotalSlots == 0 then 0 else (fromIntegral suNopSlots * 100 :: Double) / fromIntegral suTotalSlots
                percentText = toText $ showFFloat (Just 2) percent ""
             in "  VLIW nop slots (" <> label <> "): " <> percentText <> "% (" <> showT suNopSlots <> "/" <> showT suTotalSlots <> ")"
        showT :: (Show a) => a -> Text
        showT = T.pack . show
