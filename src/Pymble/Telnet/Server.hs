{-# LANGUAGE OverloadedStrings #-}
-- | This is the main module and the entry point
-- for the telnet server related functionality.
--
-- The 'startServer' pretty much does all the work to host,
-- start the server and handle the telnet clients.
--
module Pymble.Telnet.Server
  (
  -- * Telnet server bootstrap
    startServer
  -- * Client processing
  , ConnectionHandler
  , ConnectionHandlerConfig(..)
  , handleClients
  -- * Helpers
  , getAddress
  ) where

import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Network.Socket

import Pymble.AppConfig (AppConfig(..), Port)
import Pymble.Telnet.Server.ClientHandler (forkClient)
----------------------------------------------------------------------

-- | The telnet server monad for handling newly connected clients.
--
type ConnectionHandler = ReaderT ConnectionHandlerConfig IO ()


-- | The hight level telnet server configuration,
-- that is used by the 'ConnectionHandler' to
-- establish and handle client connections.
--
data ConnectionHandlerConfig = ConnectionHandlerConfig {
    _ccSocket    :: Socket      -- ^ Socket, the telnet server is listening to
  , _ccAppConfig :: AppConfig   -- ^ App startup config
  } deriving (Eq, Show)


-- | Given the application startup configuration, creates
-- and starts a telnet server on the specified port.
--
-- This is a blocking call and the control would not be
-- returned until the telnet service is terminated.
--
startServer :: AppConfig -> IO ()
startServer conf = withSocketsDo $ do
    addrInfo <- getAddress $ _appServerPort conf

    bracket (open addrInfo) close $ \sock -> do
      -- Accept and handle connections from the clients
      runReaderT handleClients (mkConfig conf sock)

  where
    -- bind and open telnet socket
    open addrInfo = do

      -- Try to get a socket to bind the telnet server listener
      sock <- socket (addrFamily addrInfo) Stream defaultProtocol

      -- This socket will accept connections from the newly connected clients
      let sockAddr = addrAddress addrInfo
      bind sock sockAddr 
      listen sock 5

      putStrLn $ "The telnet server is running on " ++ show sockAddr
      return sock

    mkConfig appConfig sock =
      ConnectionHandlerConfig {
          _ccSocket    = sock
        , _ccAppConfig = appConfig
        }


-- | The main telnet server processing loop.
-- Handles all new connections and forks a dedicated
-- thread for every single connected client.
--
handleClients :: ConnectionHandler
handleClients = forever $ do
  config <- ask

  -- accept the connection from the next telnet client
  (sock, sockAddr) <- liftIO $ accept (_ccSocket config)

  -- Using dedicated thread to handle the communication
  -- with the client.
  -- 
  -- As of now we are not tracking ThreadId, but it could
  -- be a good idea to keep track of the created threads
  -- and the clients.
  liftIO $ forkClient sock sockAddr (_ccAppConfig config)


-- | Given the port, gets an 'AddrInfo' to bind a socket.
--
getAddress :: Port -> IO AddrInfo
getAddress port =
  let hints = defaultHints {
          addrFlags = [AI_PASSIVE]
        }
  in head <$> getAddrInfo (Just hints) Nothing (Just $ show port) 