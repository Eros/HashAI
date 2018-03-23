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

}