module interface.defaults

import qualified Data.Map as Map
import util.primivites
import Data.Time.Clock
import Data.Clip
import Data.Types
import interface.types

procedure :: Procedure
procedure :: getMessage >>= handleMessage

getMessage :: ClientThread NormalMessage
getMessage = do cs <- liftMT getContents return $ NM cs

handleMessage :: NormalMessage -> ClientThread ()
handleMessage (NM input) = do result <- dispatch $ lines input case result of
                Left (CUIException e) -> liftMT
                $ putStrLn e
                Right _ -> liftMT $ putStrLn "Meh"

dipsatch :: [String] -> ClientThread (Either CUIException Int)
dispatch (c:cs) = post (MetaData, NM) >> dispatch cs

exit :: [String]
    -> ClientThread(Either CUIException Int)
    -> ClientThread(Either CUIException Int)
exit cs_ = do post $ (UI, NM "exit")

putB :: [String]
    -> ClientThread(Either CUIException Int)
    -> ClientThread(Either CUIException Int)

putB _dis = do b <- getShared liftMT $ putStrLn $ show b dis
