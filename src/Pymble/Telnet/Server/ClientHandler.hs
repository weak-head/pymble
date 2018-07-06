{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pymble.Telnet.Server.ClientHandler
  (
  -- *
    forkClient

  -- *
  , handleNewClient
  , handleRequests

  -- *
  , handleAction
  , handleCommand
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (void)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (execRWST, get, modify)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Data.String.Utils (endswith)
import Network.Socket (close, Socket, SockAddr)

import Pymble.AppConfig
import Pymble.Telnet.Api.Parser
import Pymble.Telnet.Server.Commands
import Pymble.PrettyPrint.Terminal
----------------------------------------------------------------------

-- | Telnet control sequence
type ControlSequence = [Int]


-- | Forks a dedicated thread to process the connected client
--
forkClient :: Socket        -- ^ Socket object usable to send and receive data
                            --   on the client side
           -> SockAddr      -- ^ Address bound to the socket on the other end
                            --   of the connection
           -> AppConfig     -- ^ Application configuration
           -> IO ThreadId
forkClient sock sockAddr appConfig =
    forkIO $ do
      (clientState, log) <- execRWST handleNewClient
                              (mkEnv appConfig)
                              (mkState sock sockAddr)
      -- Basically here we can save the generated log
      -- and react on the resulting client state.
      -- ATM handleClient has no actual value to return
      -- but this could be changed in the future.
      return ()

  where
    -- initializes environment
    mkEnv conf = Environment {
        _envDbConnectionInfo = _appDbConnectionInfo conf
      }

    -- initializes client state
    mkState sock sockAddr = ClientState {
        _csSocket        = sock
      , _csSockAddr      = sockAddr
      , _csConnected     = True
      , _csDefRenderConf = RenderConfig Nothing Nothing Nothing
      , _csInput         = ""
      }


-- | The main routine to handle clients
-- connected via telnet.
--
handleNewClient :: CommandHandler ()
handleNewClient = do
  writeLogStr "Connected"

  handleRequests `finally` do
    -- we want to make sure that the client socket
    -- is being closed even if some unexpected exception happens
    get >>= liftIO . close . _csSocket
    writeLogStr "Disconnected"


-- | The main client processing loop.
--
-- The result of inner loop is current 'ClientState',
-- so 'iterateWhile' can detect when the client becomes
-- disconnected.
--
handleRequests :: CommandHandler ()
handleRequests = do
    prompt
    void $ iterateWhile _csConnected $ do
      input <- unpack <$> readSocket

      modify $ append input
      acc_input <- _csInput <$> get

      let isControl   = (ord $ head input) == 255
          isCommitted = endswith "\r\n" acc_input

      case isCommitted || isControl of
        False -> return ()
        True  -> do
          handleAction $ parseAction acc_input
          clearInput >> prompt' isControl
      
      get
  where
    append s c   = c { _csInput = _csInput c ++ s }
    clearInput   = modify $ \c -> c { _csInput = "" }
    prompt' bctl = if bctl then return () else prompt
      
-- |
--
handleAction :: RequestForAction -> CommandHandler ()
handleAction = \case
  NoInput -> do
    return ()

  CRLF -> do
    return ()

  UnknownCommand input errorMsg crlf -> do
    writeSocket $ termMsg Error "Failed to parse the input"
    helpCmd

  PymbleCommand cmd crlf -> do
    handleCommand cmd

  TelnetControl controlSequence -> do
    return ()


-- |
--
handleCommand :: Command -> CommandHandler ()
handleCommand = \case
    Help            -> helpCmd
    ViewConfig      -> viewConfigCmd
    UpdateConfig rs -> setConfigCmd $ toRc rs
    Render rs url   -> renderCmd url (toRc rs)
    Quit            -> exitCmd
  where
    toRc (RenderSettings c w h) = RenderConfig c w h