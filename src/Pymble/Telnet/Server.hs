{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server (
  startServer
) where

import Network.Socket as NS
import Control.Monad.Trans.Reader as TR
import Control.Monad as CM
import Control.Exception as E
import Pymble.AppConfig


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

      -- This socket will accept the connections from all
      -- the telnet clients
      let sockAddr = NS.addrAddress addrInfo
      NS.bind sock sockAddr 
      NS.listen sock 5

      putStrLn $ "The telnet server is running on " ++ show sockAddr
      return sock

    -- creates ConnectionHandlerConfi
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
  undefined


-- | Given the port, gets address to bind
-- a socket.
--
getAddress :: Port -> IO NS.AddrInfo
getAddress port =
  let
      hints = NS.defaultHints {
          addrFlags = [NS.AI_PASSIVE]
        }
  in
    head <$> NS.getAddrInfo (Just hints) Nothing (Just $ show port) 