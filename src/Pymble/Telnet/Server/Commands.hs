{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Telnet.Server.Commands
  (
  -- * Common types
    CommandHandler
  , Log
  , Environment(..)
  , ClientState(..)
  , RenderConfig(..)

  -- * API commands
  , helpCmd
  , viewConfigCmd
  , setConfigCmd
  , renderCmd
  , exitCmd

  -- * API helpers
  , writeLogStr
  , writeMessage
  , writeNewLine
  , writePrompt

  -- * Low-level communication
  , readSocket
  , writeSocket
  , writeSocketStr
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Network.Socket (Socket, SockAddr)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Network.Socket.ByteString as NBS

import Pymble.PrettyPrint.Terminal
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
    _csSocket        :: Socket        -- ^ Socket object usable to send and receive data
                                      --   on the client connection
  , _csSockAddr      :: SockAddr      -- ^ Address bound to the socket on the other end of the
                                      --   connection
  , _csConnected     :: Bool          -- ^ True, if client is connected
  , _csDefRenderConf :: RenderConfig  -- ^ The default configuration of the ASCII art renderer
  , _csInput         :: String        -- ^ The accumulated user input
  } deriving (Eq, Show)

-- | The configuration of the ASCII art renderer.
--
data RenderConfig = RenderConfig {
    _rcColor  :: Maybe ColorScheme    -- ^ ASCII art color schema
  , _rcWidth  :: Maybe Int            -- ^ ASCII art width (in characters)
  , _rcHeight :: Maybe Int            -- ^ ASCII art height (in characters)
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
helpCmd = do
  writeMessage Info "Usage:" >> writeNewLine
  writeMessage Warning "help -> todo"
  writeNewLine


-- |
--
viewConfigCmd :: CommandHandler ()
viewConfigCmd = do
  writeMessage Warning "view config"
  writeNewLine


-- |
--
setConfigCmd :: RenderConfig
             -> CommandHandler ()
setConfigCmd rc = do
  writeMessage Warning "set config"
  writeNewLine


-- |
--
renderCmd :: String
          -> RenderConfig
          -> CommandHandler ()
renderCmd url config = do
  writeMessage Warning "render"
  writeNewLine


-- |
--
exitCmd :: CommandHandler ()
exitCmd = do
  writeMessage Warning "exit"
  writeNewLine

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


-- | Write message to the client socket.
--
writeMessage :: MessageType -> String -> CommandHandler ()
writeMessage t s = writeSocket $ termMsg t s


-- | Write new line to the client socket.
--
writeNewLine :: CommandHandler ()
writeNewLine = writeSocketStr "\r\n"


-- | Write prompt to the client socket.
--
writePrompt :: CommandHandler ()
writePrompt = writeMessage Prompt "> "

----------------------------------------------------------------------

-- | Read input as 'BS.ByteString' from the client socket.
--
readSocket :: CommandHandler BS.ByteString
readSocket = do
  sock <- _csSocket <$> get
  liftIO $ NBS.recv sock 1024


-- | Write 'ShowS' to the client socket.
--
writeSocket :: ShowS -> CommandHandler ()
writeSocket msg = writeSocketStr $ msg ""


-- | Write string to the client socket.
--
writeSocketStr :: String -> CommandHandler ()
writeSocketStr msg = do
  sock <- _csSocket <$> get
  liftIO $ NBS.sendAll sock (BSC.pack msg)