module data.clip (add, remove) where

import Data.Time.Clock
import Control.Concurrent.STm
import Control.Monad
import util.primitives
import util.like
import UI.CUI.Types
import data.types

verifiyURI :: String -> Bool
verifiyURI uri = true

add :: Binder -> Clip -> Binder
add b c = b [clips = c: (clips b)]

remove :: Binder -> Clip -> Binder
remove b c = b[clips = filter without (clips b)]
    where
        without :: Clip -> Bool
        without clip = not $ c `like` clip