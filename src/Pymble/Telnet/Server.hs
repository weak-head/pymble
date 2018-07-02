{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server
  (
  -- *
    startServer
  -- *
  , handleClients
  , getAddress
  -- *
  , ConnectionHandler
  , ConnectionHandlerConfig(..)
  ) where

import qualified Control.Concurrent         as CC
import qualified Control.Exception          as E
import qualified Control.Monad              as CM
import qualified Control.Monad.IO.Class     as CMC
import qualified Control.Monad.Trans.RWS    as MT
import qualified Control.Monad.Trans.Reader as TR
import qualified Network.Socket             as NS

import Pymble.AppConfig
import Pymble.Telnet.Server.ClientHandler
----------------------------------------------------------------------

-- | The root telnet client handler 
--
type ConnectionHandler = TR.ReaderT ConnectionHandlerConfig IO ()


-- | The configuration that is used by ConnectionHandler to
-- establish and handle client connections
--
data ConnectionHandlerConfig = ConnectionHandlerConfig {
    _ccSocket    :: NS.Socket
  , _ccAppConfig :: AppConfig
  } deriving (Eq, Show)


-- | Given the startup configuration, creates
-- and starts a telnet server on the specified port.
--
-- This is a blocking call and the control would not be
-- returned untill the telnet service is terminated.
--
startServer :: AppConfig -> IO ()
startServer conf = NS.withSocketsDo $ do
    addrInfo <- getAddress $ _appServerPort conf

    E.bracket (open addrInfo) NS.close $ \sock -> do
      -- Accept and handle connections from the clients
      TR.runReaderT handleClients (mkConfig conf sock)

  where
    -- bind and open socket
    open addrInfo = do

      -- Try to get a socket to bind the telnet server listener
      sock <- NS.socket (NS.addrFamily addrInfo) NS.Stream NS.defaultProtocol

      -- This socket will accept connections from all
      -- our telnet clients
      let sockAddr = NS.addrAddress addrInfo
      NS.bind sock sockAddr 
      NS.listen sock 5

      putStrLn $ "The telnet server is running on " ++ show sockAddr
      return sock

    mkConfig appConfig sock =
      ConnectionHandlerConfig {
          _ccSocket    = sock
        , _ccAppConfig = appConfig
        }


-- | The main entry point to handle all the connections
-- from the telnet clients.
--
handleClients :: ConnectionHandler
handleClients = CM.forever $ do
  config <- TR.ask

  -- accept the connection from the next telnet client
  (sock, sockAddr) <- CMC.liftIO $ NS.accept (_ccSocket config)

  -- Using dedicated thread to handle the communication
  -- with the client.
  -- 
  -- As of now we are not tracking ThreadId, but it could
  -- be a good idea to keep track of the created threads
  -- and the clients.
  CMC.liftIO $ forkClient sock sockAddr (_ccAppConfig config)


-- | Given the port, gets address to bind
-- a socket.
--
getAddress :: Port -> IO NS.AddrInfo
getAddress port =
  let
      hints = NS.defaultHints {
          NS.addrFlags = [NS.AI_PASSIVE]
        }
  in
    head <$> NS.getAddrInfo (Just hints) Nothing (Just $ show port) 