{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Rhyolite.Frontend.Auth.App
  ( module Rhyolite.Frontend.Auth.App
  , module X
  ) where

import qualified Rhyolite.Frontend.Auth as Base
import Rhyolite.Vessel.App as X
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Reflex.Class (Reflex)
import Reflex.Query.Class (SelectedCount)

import Rhyolite.Api (ApiRequest)
import Rhyolite.Vessel.Types (FullV, FullAuthErrorV)

-- Effectful imports
import           Effectful (Eff, (:>))
import qualified Reflex.Host.HeadlessEffectful as EFQ
import qualified Reflex.Host.HeadlessEffectful as EFRQ
import qualified Reflex.Class as Core

-- | Re-introduced MTL alias for consumers that use 'FullAppWidget'.
-- Mirrors the old MTL definition; delegates to 'Base.FullWidget'.
type FullAppWidget app =
  Base.FullWidget (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app)

-- | Re-introduced MTL alias for consumers that use 'AuthErrorAppWidget'.
type AuthErrorAppWidget app =
  Base.AuthErrorWidget (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app)

-- | Re-introduced MTL alias for consumers that use 'AuthAppWidget'.
type AuthAppWidget app =
  Base.AuthWidget (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app)


-- =============================================================================
-- Effectful port. The MTL 'authenticatedWidget' has been deleted along
-- with 'Base.authenticatedWidget'; only the Effectful port survives.
--
-- 'authenticatedWidgetE' delegates directly to 'Base.authenticatedWidgetE'
-- and ignores the 'proxy app' argument. The 'proxy app' argument is kept
-- for API-symmetry (call sites that use @Just Proxy@ / @Just \@app@ style
-- don't need to change to switch to the Effectful widget).
-- =============================================================================

-- | Effectful port of 'authenticatedWidget'. Delegates to
-- 'Base.authenticatedWidgetE'. Same shape as the MTL sibling: the
-- @proxy app@ argument fixes the app-level type-family variables
-- ('AuthCredential', 'PublicV', 'PrivateV', 'PersonalV', 'PublicApi',
-- 'PrivateApi') that would otherwise be ambiguous.
authenticatedWidgetE
  :: forall t app proxy es a.
     (
       Reflex t
     , RhyoliteAuthApp app
     , EFQ.Query t
         (FullV (AuthCredential app) (PublicV app) (PrivateV app) (PersonalV app)
                (Const SelectedCount)) :> es
     , EFRQ.Requester t
         (ApiRequest (AuthCredential app) (PublicApi app) (PrivateApi app))
         Identity :> es
     )
  => proxy app
     -- ^ App-type proxy. Same argument the MTL sibling takes; ignored
     -- structurally, used only to fix the app-level type-family
     -- variables ('AuthCredential', 'PublicV', 'PrivateV', 'PersonalV',
     -- 'PublicApi', 'PrivateApi').
  -> AuthCredential app
     -- ^ Authentication token. Same as the MTL sibling.
  -> Eff ( EFQ.Query t
             (FullAuthErrorV (PublicV app) (PrivateV app) (PersonalV app)
                             (Const SelectedCount))
         ': EFRQ.Requester t
             (ApiRequest () (PublicApi app) (PrivateApi app)) Identity
         ': es ) a
     -- ^ The inner (authenticated) child widget effect stack.
  -> Eff es a
authenticatedWidgetE _ =
  Base.authenticatedWidgetE
    @t
    @(AuthCredential app)
    @(PublicApi app)
    @(PrivateApi app)
    @(PublicV app)
    @(PrivateV app)
    @(PersonalV app)


