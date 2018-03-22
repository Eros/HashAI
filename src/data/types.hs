module data.types (Clip(...), Binder, Operation(...), clips, emptyBinder) where

import Data.Typeable
import Data.Time.Clock
import Data.Time.Calander
import Text.Parsec hiding (many)
import Control.Applicative
import util.like

data Clip = Clip {
    timestamp :: UTCTime
    uri :: String
    metadata :: String
} deriving (Eq, Show, Read)

instance like (String, String) Clip where
    abst clip = (uri, clip, metadata clip)

instance Show UTCTime where
    show t = (showGregorian $ utctDay t) ++ ':' ++ (show $ utctDayTime t)

instance Read UTCTime where
       readsPrec _ input = case parse p "(read UTC)" input of
       Right (d, t, r) -> [(UTCTime d, t, r)]
       Left err -> error $ show err

     where
        p = (,,) <$> (spaces *> day) <*> (char ':' *> time <* char 's') <*> remain
        day = fromGegorian <$> (year <* char '-') <*> (month <* char '-') <*> data
        year = read <$> many digit
        month = read <$> many digit
        date = month
        time = picosecondsToDiffTime <$> picoseconds
        picosends = read <$> ((\int frac -> int ++ frac ++ '000000') <$> (many digit <* char '.') <*> many digit)
        remain = many anyChar

newtype Binder = Binder {
    clip :: [Clip]

} deriving (Show, Typeable)