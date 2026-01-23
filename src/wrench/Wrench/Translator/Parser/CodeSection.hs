module Wrench.Translator.Parser.CodeSection (
    codeSection,
) where

import Relude
import Text.Megaparsec (choice, getSourcePos, sourceLine)
import Text.Megaparsec.Char (hspace1, string)
import Text.Megaparsec.Pos (unPos)
import Wrench.Translator.Parser.Misc
import Wrench.Translator.Parser.Types
import Wrench.Translator.Types

codeSection ::
    (MnemonicParser isa) =>
    String
    -> Parser (Section isa w String)
codeSection cstart = do
    string ".text" >> eol' cstart
    items <-
        catMaybes
            <$> many
                ( choice
                    [ nothing (hspace1 <|> eol' cstart)
                    , Just . Org <$> orgDirective cstart
                    , Just . Item <$> withLine Label label
                    , Just . Item <$> withLine Mnemonic mnemonic
                    ]
                )
    return $ Code (sectionOrg items) $ sectionItems items

withLine :: (a -> Maybe Int -> b) -> Parser a -> Parser b
withLine ctor parser = do
    pos <- getSourcePos
    x <- parser
    return $ ctor x (Just $ unPos $ sourceLine pos)
