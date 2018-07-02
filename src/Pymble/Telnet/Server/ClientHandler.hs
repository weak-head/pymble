{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server.ClientHandler
  (
  -- *
    forkClient
  -- *
  , serveClient
  , handleClientRequests
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (void)
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.Trans.RWS (execRWST, get)
import Network.Socket (close, Socket, SockAddr)

import Pymble.AppConfig
import Pymble.Telnet.Server.Commands
----------------------------------------------------------------------


-- | Forks a dedicated thread to process the connected client
--
forkClient :: Socket
           -> SockAddr
           -> AppConfig
           -> IO ThreadId
forkClient sock sockAddr appConfig =
    forkIO $ do
      (clientState, log) <- execRWST serveClient
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
        _csConnected  = True
      , _csSocket     = sock
      , _csSockAddr   = sockAddr
      }


-- | The main routine to handle clients
-- connected via telnet.
--
serveClient :: CommandHandler ()
serveClient = do
  writeLogStr "Connected"

  handleClientRequests `finally` do
    -- we want to make sure that the client socket
    -- is being closed even if some unexpected exception happens
    get >>= liftIO . close . _csSocket
    writeLogStr "Disconnected"


-- | The main client processing loop.
--
handleClientRequests :: CommandHandler ()
handleClientRequests = void $ iterateWhile _csConnected $ do
  undefined

  -- final ClientState is the result of our monad,
  -- so iterateWhile can detect
  -- when the client is disconnected
  get