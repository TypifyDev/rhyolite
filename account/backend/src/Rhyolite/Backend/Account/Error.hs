{-|
Description:
  Typed errors for the Rhyolite account backend.

  Introduced so callers can pattern-match on failure conditions rather than
  having the library 'error' out into an unrecoverable runtime crash. See
  'Rhyolite.Backend.Account.Db' for the sites that now return these instead
  of calling 'error'.
-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
module Rhyolite.Backend.Account.Error
  ( AccountError (..)
  , handleAccountError
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Failure modes that were previously encoded as 'error' calls inside the
-- account backend. Each constructor documents when it fires so downstream
-- code can pattern-match with intent.
data AccountError
  = -- | The database refused (or returned an unexpected shape for) an
    -- account-creation insert. Payload is the operation label so callers can
    -- log which site tripped.
    AccountCreationFailed Text
  | -- | Lookup expected a row that turned out not to exist.
    AccountNotFound Text
  deriving (Show, Eq, Generic)

-- | Convenience wrapper for the common "log-and-swallow" caller pattern.
-- Consumers who really do want to crash on an 'AccountError' should
-- pattern-match themselves; this helper is just for the boring log-then-drop
-- case so we don't repeat it at every call site.
handleAccountError
  :: Applicative m
  => (AccountError -> m a)   -- ^ what to do on failure
  -> (b -> m a)              -- ^ what to do on success
  -> Either AccountError b
  -> m a
handleAccountError onErr onOk = either onErr onOk
