module utility.mthread (ThreadMessage(...), GenMThread, MThread, forkMT, runMT, liftMT, getShared, setShared, modifiyShared,
    getStatus, setStatus, modifyStatus, fetch, tryFetch, post, require, killMThread) where

   import qualified Data.Map as Map
   import Control.Monad
   import Control.Monad.State
   import Control.Concurrent
   import Control.Concurrent.STm

class ThreadMessage msg tm where
    code :: tim -> msg
    decode :: msg -> tm

instance (ThreadMessage msg tm) => ThreadMessage(Maybe msg) (Maybe tm) where
