{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{-|
Description: Effectful replacement for the MTL 'Rhyolite.Frontend.App.RhyoliteWidget'
newtype-shape — expressed as a constraint synonym over an @Eff es a@ effect row
instead of a newtype wrapper.

= What this module provides

'RhyoliteWidgetC' — a constraint synonym that names, in one place, the two
obligations the deleted MTL 'RhyoliteWidget' bundled on top of its underlying
monad:

  1. Query capability — a @'EFQ.Query' t q@ effect member (view-selector queries).
  2. Requester capability — a @'EFRQ.Requester' t r 'Identity'@ effect member
     (request emission with 'Identity'-parametric responses; matches the MTL
     shape @RequesterT t r Identity m@).

Consumers write @'Eff' es a@ with @'RhyoliteWidgetC' q r t es@ in their type
signature. There is no wrapper to construct or destructure — those two effects
sit natively in the effect row.

= Why a constraint synonym, not a newtype

The MTL 'Rhyolite.Frontend.App.RhyoliteWidget' newtype existed for one reason:
'MTL' resolves capabilities by picking an @instance MonadFoo (SomeMonad m)@
via the type of the top-level monad. Bundling @QueryT t q@ + @RequesterT t r
Identity@ into a single named wrapper let a downstream author say "any @m@
that is a @MonadFoo@" and have @RhyoliteWidget@ dispatch it through its
hand-written instance list ('Requester', 'Query', 'DomBuilder', 'PostBuild',
…). That whole pattern is redundant in the Effectful world — effects live
side-by-side in the row and are dispatched by @':>' es@ membership, not by
type-of-monad. Wrapping them again would add constructors\/deconstructors
and buy zero additional type-safety.

Grep evidence: every use of 'RhyoliteWidget' in the working tree
('frontend', 'backend', 'common', 'thunks') is either (a) a comment or
docstring describing the deleted MTL surface, or (b) source inside
@deps\/jenga\/deps\/rhyolite@ that is being ported away. No consumer in the
Effectful codebase treats the wrapper as a distinct type for instance
dispatch, so there is no semantic reason to keep the newtype-shape. The
already-ported 'Rhyolite.Frontend.Auth.authenticatedWidgetE' and
'Rhyolite.Frontend.Effect.WidgetRunner.mapAuthE' both operate directly on
raw effect rows — this synonym simply gives that shape a memorable name.

= Relationship to the deleted MTL surface

MTL:

@
newtype 'RhyoliteWidget' q r t m a = 'RhyoliteWidget'
  { 'unRhyoliteWidget' :: 'QueryT' t q ('RequesterT' t r 'Identity' m) a }
@

Effectful (this module):

@
type 'RhyoliteWidgetC' q r t es =
  ( 'EFQ.Query' t q ':>' es
  , 'EFRQ.Requester' t r 'Identity' ':>' es
  )
@

The port is deliberately narrow: exactly the two obligations the MTL wrapper
introduced (queries + requester), nothing more. Any additional widget
capability — 'Reflex.Effectful.Dom.Builder.Class', 'PostBuild',
'PerformEvent', 'TriggerEvent', 'Hold', 'JSM'' — comes from the outer
'Eff' row via the app root's interpreter stack (typically 'mainWidgetEff'
\/ 'Jenga.Frontend.Static.runFrontend'), just like it did for the MTL
underlying-monad @m@. Those effects are orthogonal to whether the widget is
"a Rhyolite widget" and belong on the consumer's own signature (or the
umbrella constraint the consumer already imports), not on this synonym.
-}
module Rhyolite.Frontend.Effect.Widget
  ( RhyoliteWidgetC
  ) where

import           Data.Functor.Identity        (Identity)
import           Effectful                    ((:>))

import qualified Reflex.Host.HeadlessEffectful as EFQ
import qualified Reflex.Host.HeadlessEffectful as EFRQ


-- | Constraint synonym replacing the MTL 'Rhyolite.Frontend.App.RhyoliteWidget'
-- newtype.
--
-- A widget that "is a rhyolite widget" is one whose effect row @es@ can
-- issue view-selector queries in currency @q@ and can emit requests in
-- currency @r@ (with 'Identity'-parametric responses — matching the MTL
-- @RequesterT t r Identity m@ shape).
--
-- Usage:
--
-- @
-- myWidget
--   :: 'RhyoliteWidgetC' MyQuery MyReq t es
--   => 'Eff' es ()
-- myWidget = do
--   … 'watch' \/ 'requesting' etc. …
-- @
--
-- Note: consumers that also need widget-shell capabilities ('Dom',
-- 'PostBuild', 'PerformEvent', 'Hold', …) should add those effect
-- constraints alongside — they are provided by the app-root interpreter
-- stack and are orthogonal to the Query\/Requester obligations this
-- synonym names.
type RhyoliteWidgetC q r t es =
  ( EFQ.Query t q :> es
  , EFRQ.Requester t r Identity :> es
  )
