{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Wrench.Translator.Types (
    Section (..),
    SectionKind (..),
    SectionInfo (..),
    CodeToken (..),
    SourceInfo (..),
    DataToken (..),
    DataValue (..),
    ByteSize (..),
    MachineWord,
    markupOffsets,
    markupSectionOffsets,
    DerefMnemonic (..),
    deref',
    Ref (..),
    derefSection
    ) where

import Data.Aeson (ToJSON)

import Relude
import Wrench.Machine.Types
import Prelude qualified

class DerefMnemonic m w where
    derefMnemonic :: (String -> Maybe w) -> w -> m (Ref w) -> m w

data Section isa w l
    = Code
        { org :: Maybe Int
        , codeTokens :: ![CodeToken isa l]
        }
    | Data
        { org :: Maybe Int
        , dataTokens :: ![DataToken w l]
        }
    deriving (Show)

data SectionKind = TextKind | DataKind
    deriving (Eq, Show)

data SectionInfo = SectionInfo
    { siOffset :: Int
    , siSize :: Int
    , siKind :: SectionKind
    }
    deriving (Show)

instance (ByteSize isa, ByteSizeT w) => ByteSize (Section isa w l) where
    byteSize Code{codeTokens} = sum $ map byteSize codeTokens
    byteSize Data{dataTokens} = sum $ map byteSize dataTokens

derefSection ::
    forall isa w.
    (ByteSize (isa (Ref w)), DerefMnemonic isa w, MachineWord w) =>
    (String -> Maybe w)
    -> w
    -> Section (isa (Ref w)) w String
    -> Section (isa w) w w
derefSection f offset code@Code{codeTokens} =
    let mnemonics = [(ctLine, m) | Mnemonic{ctMnemonic = m, ctLine} <- codeTokens]
        (_finalOffset, marked) =
            mapAccumL
                ( \curOffset (mLine, mnem) ->
                    let start = curOffset
                        curOffset' = curOffset + toEnum (byteSize mnem)
                     in (curOffset', (start, mLine, mnem))
                )
                offset
                mnemonics
     in code
            { codeTokens =
                map
                    ( \(offset', mLine, mnem) ->
                        let m' = derefMnemonic f offset' mnem
                         in Mnemonic{ctMnemonic = m', ctLine = mLine}
                    )
                    marked
            }
derefSection f _offset dt@Data{dataTokens} =
    dt
        { dataTokens =
            map
                ( \DataToken{dtLabel, dtValue} ->
                    DataToken
                        { dtLabel = fromMaybe (error $ "unknown label: " <> show dtLabel) $ f dtLabel
                        , dtValue = dtValue
                        }
                )
                dataTokens
        }

markupOffsets :: (ByteSize t, MachineWord w) => w -> [t] -> [(w, t)]
markupOffsets _offset [] = []
markupOffsets offset (m : ms) = (offset, m) : markupOffsets (offset + toEnum (byteSize m)) ms

markupSectionOffsets :: (ByteSize isa, MachineWord w) => w -> [Section isa w l] -> [(w, Section isa w l)]
markupSectionOffsets _offset [] = []
markupSectionOffsets offset (s : ss) =
    let offset' = Prelude.maybe offset toEnum (org s)
     in (offset', s) : markupSectionOffsets (offset' + toEnum (byteSize s)) ss

data CodeToken isa l
    = Label
        { ctLabel :: l
        , ctLine :: Maybe Int
        }
    | Mnemonic
        { ctMnemonic :: isa
        , ctLine :: Maybe Int
        }
    deriving (Show, Generic)

instance (ByteSize isa) => ByteSize (CodeToken isa l) where
    byteSize Mnemonic{ctMnemonic} = byteSize ctMnemonic
    byteSize Label{} = 0

data SourceInfo = SourceInfo
    { siLine :: !Int
    , siText :: !Text
    }
    deriving (Show, Generic, ToJSON)

data Ref w
    = Ref (w -> w) String
    | ValueR (w -> w) w

instance (Eq w) => Eq (Ref w) where
    (Ref _ l) == (Ref _ l') = l == l'
    (ValueR _ x) == (ValueR _ x') = x == x'
    _ == _ = False

instance (Show w) => Show (Ref w) where
    show (Ref _ l) = l
    show (ValueR f x) = show $ f x

deref' :: (String -> Maybe w) -> Ref w -> w
deref' f (Ref prepare l) = prepare <$> fromMaybe (error ("Can't resolve label: " <> show l)) $ f l
deref' _f (ValueR prepare x) = prepare x

data DataToken w l = DataToken
    { dtLabel :: !l
    , dtValue :: DataValue w
    }
    deriving (Show)

instance (ByteSizeT w) => ByteSize (DataToken w l) where
    byteSize DataToken{dtValue} = byteSize dtValue

data DataValue w
    = DByte [Word8]
    | DWord [w]
    deriving (Show)

instance (ByteSizeT w) => ByteSize (DataValue w) where
    byteSize (DByte xs) = length xs
    byteSize (DWord xs) = byteSizeT @w * length xs
