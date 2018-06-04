{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server.ClientHandler (

) where

import Control.Monad.Trans.RWS as MT
----------------------------------------------------------------------

-- | Encapsulates all the necessary
-- environment configuration and information.
--
data Environment = Environment {

  } deriving (Eq, Show)


-- | The log of all interactions with a client.
--
data Log = Log {

  } deriving (Eq, Show)


-- | Reflects the state of the session
-- and the connected client.
--
data ClientState = ClientState {

  } deriving (Eq, Show)


-- | The telnet client handler monad
-- with an access to the environment information,
-- that is capable of accumulating log entries and
-- preserves the client connection state.
--
type ClientHandler a = MT.RWST Environment Log ClientState IO a

----------------------------------------------------------------------


