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

  -- * API Commands
  , helpCmd
  , viewConfigCmd
  , setConfigCmd
  , renderCmd
  , exitCmd

  -- * Command helpers
  , writeLogStr

  -- * Low-level communication
  , readSocket
  , writeSocket
  , writeSocketStr
  , newLine
  , prompt
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
  let usage = termMsg Info "\r\nUsage:"
      help  = termMsg Warning "\r\nhelp -> todo"
  writeSocket $ usage . help
  newLine


-- |
--
viewConfigCmd :: CommandHandler ()
viewConfigCmd = do
  writeSocket $ termMsg Warning "view config"
  newLine


-- |
--
setConfigCmd :: RenderConfig
             -> CommandHandler ()
setConfigCmd rc = do
  writeSocket $ termMsg Warning "set config"
  newLine


-- |
--
renderCmd :: String
          -> RenderConfig
          -> CommandHandler ()
renderCmd url config = do
  writeSocket $ termMsg Warning "render"
  newLine


-- |
--
exitCmd :: CommandHandler ()
exitCmd = do
  writeSocket $ termMsg Warning "exit"
  newLine

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


-- | Write new line to the client socket.
--
newLine :: CommandHandler ()
newLine = writeSocketStr "\r\n"


-- | Write prompt to the client socket.
--
prompt :: CommandHandler ()
prompt = writeSocket $ termMsg Success "> "