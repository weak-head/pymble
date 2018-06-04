{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server.ClientHandler (
    Environment(..)
  , Log
  , ClientState(..)
  , ClientHandler

  -------------------
  , handleClient

) where

import Network.Socket as NS
import Control.Monad (void)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.RWS as MT

----------------------------------------------------------------------

-- | Encapsulates all the necessary
-- environment configuration and information.
--
data Environment = Environment {
    _envDbConnectionInfo :: String
  } deriving (Eq, Show)


-- | The log of all interactions with a client.
--
-- Temporary using [String] to satisfy monoid laws
--
type Log = [String]


-- | Reflects the state of the session
-- and the connected client.
--
data ClientState = ClientState {
    _csSocket     :: NS.Socket
  , _csSockAddr   :: NS.SockAddr
  , _csConnected  :: Bool
  } deriving (Eq, Show)


-- | The telnet client handler monad
-- with an access to the environment information,
-- that is capable of accumulating log entries and
-- preserves the client connection state.
--
type ClientHandler a = MT.RWST Environment Log ClientState IO a

----------------------------------------------------------------------


-- | The main client processing loop.
--
handleClient :: ClientHandler ()
handleClient = void $ iterateWhile _csConnected $ do
  undefined
  get

