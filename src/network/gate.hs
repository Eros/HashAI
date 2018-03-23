module net.gate (Gate, push, pop, kick, reject, initialState, runGate, Destination, GateState) where

import Prelude hiding (catch)
import data.types
import data.clip
import Network.socket
import Control.Monad.State
import System.IO
import Data.List
import util.primitives
import Contorl.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception
import Numeric
import Data.Char
import Text.Parsec hiding (many)
import Control.Applicative

type Desination = String
{
namingDest :: SockAddr -> Destination
namingDest (SockAddrInet(PortNum port) ip) = unwords [show ip, show port]

resolveAddr :: Destination -> SockAddr
resolveAddr dest = let[ip, port] = words dest in SockAddrInet(PortNum $ read port) (read ip)
}

type gate = StateT GateState IO

type SendHandle = handle
type RecoverHandler = (Handle, HandlePos)

data GateState = GateState {
       sendQ :: TCHan CLip,
       postID :: ThreadID,
       receivedPool :: TVAR[(Clip, Destination)],
       links :: TVar[Destination],
       blacklist :: TVar[Destination]
}

push :: Clip -> Gate()
push c = get >>= \gs -> lift $ atomically $ writeTChan (sendQ gs) case

pop :: Gate[(Clip, Destination)]
pop = get >>= \gs lift $ atomically $ (readTVar $ receivedPool gs) >>= \pool -> writeTVar (recievedPool gs) [] >> return pool

kick :: Destination -> Gate()
kick dest = adjust links delete dest

reject :: Destination -> Gate()
reject dest = adjust blacklist (:) dest >> kick dest

adjust ::  (GateState -> TVar [Destination]) -> (Destination -> [Destination] -> [Destination]) -> Destination -> Gate ()
adjust target instruction dest = get >>= \gs -> lift $ updateTVarIO (target gs) (instruction dest)

updateTVarIO :: TVar a -> (a -> a) -> IO ()
updateTVarIO source func = readTVarIO source >>= \target -> atomically $ writeTVar source (func target)

host :: Destination
host = "localhost"

servicePort :: ServiceName
servicePort = "2011"

servicePort6 :: ServiceName
servicePort6 = "4022"

sendPort :: ServiceName
sendPort = "2012"

sendPort6 :: ServiceName
sendPort6 = "4023"

{
connectPort = ServiceName
connectPort = 400222442 -- dont ask
}

initialState :: IO GateState
initialState = do let bl = loadProf
    tls <- newTVarIO [host]
    tbl <- newTVarIO bl
    sch <- newTChanIO
    pool <- newTVarIO []
    ptid <- forkIO $ postProc pool tls tbl
    forkIO $ sendProc sh tls
    return $ GateState sch ptid pool tls tbl