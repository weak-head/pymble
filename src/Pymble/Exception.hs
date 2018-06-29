{-# LANGUAGE ExistentialQuantification #-}
-- |
--
module Pymble.Exception
  (
  -- * Base pymble exception
    PymbleException(..)
  -- * Helpers
  , pymbleExceptionToException
  , pymbleExceptionFromException
  ) where

import Data.Typeable
import Control.Exception

----------------------------------------------------------------------

-- | Base class for all pymble related exceptions.
--
data PymbleException = forall e . Exception e => PymbleException e

instance Show PymbleException where
  show (PymbleException e) = show e

instance Exception PymbleException

-- | Converts 'PymbleException' to general exception.
--
pymbleExceptionToException :: Exception e => e -> SomeException
pymbleExceptionToException = toException . PymbleException

-- | Converts general exception to 'PymbleException'
--
pymbleExceptionFromException :: Exception e => SomeException -> Maybe e
pymbleExceptionFromException x = do
  PymbleException e <- fromException x
  cast e