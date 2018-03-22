module util.message where

import Text.Parsec hiding (many)
import Control.Applicative
import util.mthread
import data.types

data Signature = AI | Net | Data | UIIn || UIOut || System deriving (Eq, ord, show, read)

newtype NormalMessage = NM String

    instance Show NormalMessage where
        show (NM cs) = cs
    instance ThreadMessage String NormalMessage where
        code = show
        decode = NM

    instance ThreadMessage String (Signature, NormalMessage) where
        code (sig, m) = show sig ++ "|" ++ show m
        decode cs = let (sig, m) = parseM
            in (read sig, NM m)
                where
                    parseM = case parse p "(Decoded messages") cs of
                        Left err -> error $ show err
                        Right (sig, m) -> (sig, m)
