{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
  , logInfo
  , writeMessage
  , writeMessageLn
  , writeNewLine
  , writePrompt

  -- * Low-level communication
  , readSocket
  , writeSocket
  , writeSocketStr
  ) where

import Control.Monad.Catch (catches)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Network.Socket (Socket, SockAddr)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Network.Socket.ByteString as NBS

import Pymble.Image.Convert (normalize, toDelayedAsciiArt)
import Pymble.Image.Fontspec (courierFull)
import Pymble.Image.Helpers (adviceSize, imageSize)
import Pymble.Image.Storage (download, defLoadHandlers)
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


-- | Output pymble help info.
--
helpCmd :: CommandHandler ()
helpCmd = do
    -- Usage header ---------------------------- 
    info "Usage: " >> cmd "<command> "
      >> info "[" >> arg "<args>" >> info "]"
      >> nl >> nl

    -- Commands -------------------------------
    info "Available commands:"
      >> nl

    cmd "  help" 
      >> al 28 >> info "Show this help text"
      >> nl

    cmd "  config "
      >> info "[" >> arg "<args>" >> info "]"
      >> al 17 >> info "Show/update default renderer config"
      >> nl

    cmd "  r" >> info " | " >> cmd "render "
      >> info "[" >> arg "<args>" >> info "] " >> info "URL"
      >> al 9 >> info "Convert image to ASCII art"
      >> nl

    cmd "  q" >> info " | " >> cmd "quit" >> info " | " >> cmd "exit"
      >> al 17 >> info "Close the session"
      >> nl

    -- Command args ----------------------------
    nl
    info "Available arguments:"
      >> nl

    arg "  w" >> info " | " >> arg "width "
      >> hint " INT"
      >> al 18 >> info "ASCII art width"
      >> nl

    arg "  h" >> info " | " >> arg "height "
      >> hint "INT"
      >> al 18 >> info "ASCII art height"
      >> nl 

    arg "  c" >> info " | " >> arg "color "
      >> hint " PALETTE"
      >> al 14 >> info "ASCII art color palette" >> nl
        >> al 16 >> hint "16"
            >> al 16 >> info "Standard 16-color map"
            >> nl
        >> al 16 >> hint "256"
            >> al 15 >> info "Standard 8-bit (256 color) map"
            >> nl
        >> al 16 >> hint "gs" >> info " | " >> hint "grayscale"
            >> al 4 >> info "Grayscale subset of 8-bit color map"
            >> nl
        >> al 16 >> hint "tc" >> info " | " >> hint "truecolor"
            >> al 4 >> info "24-bit TrueColor"
            >> nl
      >> nl

    -- Usage examples --------------------------
    info "Usage examples:"
      >> nl

    al 2 >> p >> cmd "config"
      >> al 24 >> info "Show default config"
      >> nl

    al 2 >> p >> cmd "config "
      >> arg "c " >> hint "TrueColor " 
      >> arg "w " >> hint "40"
      >> al 7 >> info "Set default config"
      >> nl

    al 2 >> p >> cmd "r "
      >> info "http://some.url/img.png"
      >> al 5 >> info "Convert using default config"
      >> nl

    al 2 >> p >> cmd "r "
      >> arg "color " >> hint "256 "
      >> info "http://img.png"
      >> al 4 >> info "Convert using explicit config"
      >> nl

    nl
  where
    info = writeMessage Info
    hint = writeMessage Hint
    cmd  = writeMessage Command
    arg  = writeMessage CommandArg
    nl   = writeNewLine
    al n = info $ replicate n ' ' 
    p    = writePrompt


-- | Show default ASCII art renderer config.
--
viewConfigCmd :: CommandHandler ()
viewConfigCmd = do
    rc <- _csDefRenderConf <$> get
    arg "Color palette:    " >> clr rc >> nl
    arg "ASCII art width:  " >> wdt rc >> nl
    arg "ASCII art height: " >> hgt rc >> nl
    nl
  where 
    arg   = writeMessage CommandArg
    info  = writeMessage Info
    nl    = writeNewLine
    clr c = info $ showColor $ maybe Color16 id (_rcColor c)
    wdt c = info $ maybe "auto" showSize (_rcWidth c)
    hgt c = info $ maybe "auto" showSize (_rcHeight c)
    showColor = \case
      Color16   -> "16 colors (4-bit)"
      Xterm256  -> "256 colors (8-bit)"
      Grayscale -> "8-bit grayscale"
      TrueColor -> "True color (24-bit)"
    showSize n = show n ++ " chars"


-- | Set default ASCII art renderer config.
--
setConfigCmd :: RenderConfig
             -> CommandHandler ()
setConfigCmd rc = do
  modify $ \c -> c { _csDefRenderConf = rc }
  viewConfigCmd


-- | Render ASCII art to terminal.
--
renderCmd :: String
          -> RenderConfig
          -> CommandHandler ()
renderCmd url config = do
    (RenderConfig c w h) <- merge config <$> _csDefRenderConf <$> get

    -- todo: write log
    hint "Downloading image..."
    maybeImage <- liftIO $ (normalize <$> download url)
                    `catches` defLoadHandlers

    case maybeImage of
      Right image -> do
        let isize@(iw, ih)  = imageSize image
            (width, height) = adviceSize isize w h
            color           = maybe Color16 id c
            delayedArt      = toDelayedAsciiArt width height courierFull image

        -- todo: write log
        hint $ concat [ "Converting "
                      , show iw ++ "x" ++ show ih ++ " image to "
                      , show width ++ "x" ++ show height ++ " ASCII art..."
                      ] 
        coloredArt <- liftIO $ evalAsTerminalColor color delayedArt

        nl >> nl
        writeSocket $ prettyPrint coloredArt
        nl >> nl

      Left err -> do
        -- todo: write log
        writeMessageLn Error err
  where
    -- merges explicitly specified and the default render configs,
    -- on conflict priority goes to the first one 
    (RenderConfig c w h) `merge` (RenderConfig c' w' h') =
      RenderConfig
        (maybe c' Just c)
        (maybe w' Just w)
        (maybe h' Just h)

    hint  = writeMessageLn Hint
    nl    = writeNewLine


-- | Close the connection to the server.
--
exitCmd :: CommandHandler ()
exitCmd = do
  modify $ \c -> c { _csConnected = False }
  writeNewLine

----------------------------------------------------------------------

-- | Append string to log.
--
logInfo :: String -> CommandHandler ()
logInfo message = do
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


-- | The same as 'writeMessage', but adds a newline character.
writeMessageLn :: MessageType -> String -> CommandHandler ()
writeMessageLn t s = writeMessage t s >> writeNewLine


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