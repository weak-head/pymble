{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server.ClientHandler (
    Environment(..)
  , Log
  , ClientState(..)
  , ClientHandler

  -------------------
  , serveClient

) where

import Network.Socket                     as NS
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.RWS            as MT
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)

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


-- | The main routine to handle clients
-- connected via telnet.
--
serveClient :: ClientHandler ()
serveClient = do
  writeLogStr "Connected"

  handleClientRequests

  get >>= liftIO . close . _csSocket
  writeLogStr "Disconnected"


-- | The main client processing loop.
--
handleClientRequests :: ClientHandler ()
handleClientRequests = void $ iterateWhile _csConnected $ do
  undefined
  get

----------------------------------------------------------------------

writeLogStr :: String -> ClientHandler ()
writeLogStr message = do
  now  <- show <$> liftIO getCurrentTime
  addr <- show . _csSockAddr <$> get

  -- idealy we dont want to use [String] for the log
  -- and we want to replace it with some appropriate
  -- implementation, but for now we work with slow
  -- linked list and ugly strings.
  let logEntry = intercalate " | "
                  [ now
                  , addr
                  , message
                  ]

  liftIO $ putStrLn logEntry

  -- this forces us to append to the end of linked list
  -- every single time... extremely unefficient.
  tell [logEntry]