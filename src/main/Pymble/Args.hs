{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Args
    ( 
    -- *
      StartupMode(..)
    , startupMode
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

import Pymble.AppConfig (Port)
import Pymble.PrettyPrint.Telnet (ColorScheme(..))
----------------------------------------------------------------------

-- |
--
type Width  = Int


-- |
--
type Height = Int


-- |
--
type Url = String


-- |
--
data StartupMode =
    TelnetServer (Maybe Port)
  | Convert (Maybe Width) (Maybe Height) (Maybe ColorScheme) Url


-- |
--
startupMode :: Parser StartupMode
startupMode = undefined
