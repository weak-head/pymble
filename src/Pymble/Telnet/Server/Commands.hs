{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Telnet.Server.Commands
  (
  -- * Common commands data
    CommandHandler
  , Log
  , Environment(..)
  , ClientState(..)
  -- * Commands
  , helpCmd
  , viewConfigCmd
  , setConfigCmd
  , renderCmd
  , exitCmd
  -- * Command helpers
  , writeLogStr
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Network.Socket (Socket, SockAddr)
----------------------------------------------------------------------

-- | The telnet command handler monad
-- with an access to the environment information,
-- that is capable of accumulating log entries and
-- preserves the client connection state.
--
type CommandHandler a = RWST Environment Log ClientState IO a

-- | Reflects the state of the session
-- and the connected client.
--
data ClientState = ClientState {
    _csSocket     :: Socket
  , _csSockAddr   :: SockAddr
  , _csConnected  :: Bool
  } deriving (Eq, Show)

-- | Log of all interactions with a client.
--
type Log = ShowS

-- | Encapsulates all the necessary
-- environment configuration and information.
--
data Environment = Environment {
    _envDbConnectionInfo :: String
  } deriving (Eq, Show)

----------------------------------------------------------------------


-- |
--
helpCmd :: CommandHandler ()
helpCmd = undefined


-- |
--
viewConfigCmd :: CommandHandler ()
viewConfigCmd = undefined 


-- |
--
setConfigCmd :: CommandHandler ()
setConfigCmd = undefined


-- |
--
renderCmd :: CommandHandler ()
renderCmd = undefined


-- |
--
exitCmd :: CommandHandler ()
exitCmd = undefined

----------------------------------------------------------------------

-- |
--
writeLogStr :: String -> CommandHandler ()
writeLogStr message = do
  now  <- show <$> liftIO getCurrentTime
  addr <- show . _csSockAddr <$> get

  -- ideally we dont want to use ShowS for the log
  -- and we want to replace it with some appropriate
  -- implementation, but for now we go with it.
  let logEntry = intercalate " | "
                  [ now
                  , addr
                  , message
                  ]

  liftIO $ putStrLn logEntry
  tell $ showString logEntry