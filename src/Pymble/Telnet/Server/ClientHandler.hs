{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pymble.Telnet.Server.ClientHandler
  (
  -- *
    forkClient

  -- *
  , handleConnectedClient
  , processClientRequests

  -- *
  , commandHandler
  , parsingErrorHandler
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (void)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.RWS (execRWST, get)
import Control.Monad.Trans.Class (lift)
import Network.Socket (close, Socket, SockAddr)

import Pymble.AppConfig
import Pymble.Telnet.Api.Parser
import Pymble.Telnet.Server.Commands
----------------------------------------------------------------------


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
      (clientState, log) <- execRWST handleConnectedClient
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
      }


-- | The main routine to handle clients
-- connected via telnet.
--
handleConnectedClient :: CommandHandler ()
handleConnectedClient = do
  writeLogStr "Connected"

  processClientRequests `finally` do
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
processClientRequests :: CommandHandler ()
processClientRequests =
    void $ iterateWhile _csConnected $ do
      readCommand >>= handleCommand >> get
  where
    -- Get input from the client and parse it as 'Command'
    readCommand = readSocket >>= lift . return . parseCommandBS
    -- Handle parsing error or command
    handleCommand = \case
      Left err  -> parsingErrorHandler err
      Right cmd -> commandHandler cmd


-- |
--
commandHandler :: Command -> CommandHandler ()
commandHandler = \case
  Help         -> helpCmd
  ViewConfig   -> viewConfigCmd
  UpdateConfig -> setConfigCmd
  Render       -> renderCmd "" (RenderConfig Nothing Nothing Nothing)
  Quit         -> exitCmd


-- |
--
parsingErrorHandler :: ParsingError -> CommandHandler ()
parsingErrorHandler = undefined