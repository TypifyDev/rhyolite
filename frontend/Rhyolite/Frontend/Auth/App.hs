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
import Reflex.PostBuild.Class (PostBuild)
import Control.Monad.Fix (MonadFix)
import Reflex.Query.Class (SelectedCount)

import Rhyolite.Api (ApiRequest)
import Rhyolite.Vessel.Types (FullV, FullAuthErrorV)

-- Effectful imports
import           Effectful (Eff, (:>))
import qualified Reflex.Host.HeadlessEffectful as EFQ
import qualified Reflex.Host.HeadlessEffectful as EFRQ

-- | MTL widget with public and authenticated requests and queries.
type FullAppWidget app =
  Base.FullWidget (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app)

-- | MTL widget whose queries may return authentication errors.
type AuthErrorAppWidget app =
  Base.AuthErrorWidget (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app)

-- | MTL widget after authentication errors have been handled.
type AuthAppWidget app =
  Base.AuthWidget (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app)


-- | Specialize an MTL child widget to an authentication token.
-- See 'authenticatedWidgetE' for the Effectful counterpart.
authenticatedWidget
  :: (MonadFix m, PostBuild t m, RhyoliteAuthApp app)
  => proxy app
  -> AuthCredential app
  -> AuthErrorAppWidget app t m a
  -> FullAppWidget app t m a
authenticatedWidget _ = Base.authenticatedWidget

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
