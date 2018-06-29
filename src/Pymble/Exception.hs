{-# LANGUAGE ExistentialQuantification #-}
-- |
--
module Pymble.Exception
  (
  -- * Base pymble exception
    PymbleException(..)
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

pymbleExceptionToException :: Exception e => e -> SomeException
pymbleExceptionToException = toException . PymbleException

pymbleExceptionFromException :: Exception e => SomeException -> Maybe e
pymbleExceptionFromException x = do
  PymbleException e <- fromException x
  cast e