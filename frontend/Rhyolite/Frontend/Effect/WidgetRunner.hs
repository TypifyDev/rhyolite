{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Effectful bridges for the MTL widget-runner primitives in
"Rhyolite.Frontend.App" (@runQueryT@, @withQueryT@, @withRequesterT@,
@mapAuth@).

= What this module provides

The E-suffixed siblings of the MTL widget-runner helpers that can be
honestly ported without reaching into reflex-internal machinery:

  * 'runQueryTE'       — Effectful port of 'Reflex.Query.Base.runQueryT'
  * 'withQueryTE'      — Effectful port of 'Reflex.Query.Base.withQueryT'
  * 'withRequesterTE'  — Effectful port of 'Reflex.Requester.Base.withRequesterT'
  * 'mapAuthE'         — Effectful port of 'Rhyolite.Frontend.App.mapAuth'

'runQueryTE' \/ 'withQueryTE' are implemented by reinterpreting the
'Reflex.Host.HeadlessEffectful.Query' effect against a private
'Effectful.Reader.Static.Reader' (ambient 'Dynamic' 'QueryResult') plus
a private 'Effectful.State.Static.Local.State' collector that mirrors
the '[Behavior t q]' \/ tellQueryDyn accumulator inside
'Reflex.Query.Base.QueryT'. At the boundary we materialise the
collected state into the same 'Incremental' the MTL 'runQueryT'
returns, using only public reflex primitives ('unsafeBuildIncremental',
'mergeWith', 'sample', 'foldlM').

'withRequesterTE' is implemented as a straight pass-through
reinterpretation. The 'Requester' effect in @reflex-dom-effectful@ operates
in a caller-provided 'GCompare' 'DMap' currency — @Event t (DMap k
req)@ in, @Event t (DMap k rsp)@ out — with NO internal tag routing
(unlike the MTL 'Reflex.Requester.Base.RequesterT', which relies on
'MyTag' \/ 'MyTagWrap' \/ 'TagMap' \/ 'RequesterData' plumbing). Because
the caller owns the keys, mapping the request currency (@req -> req'@)
and response currency (@rsp' -> rsp@) is a pure 'Data.Dependent.Map.map'
under the event functor — no unsafe machinery needed. The interpreter
delegates to the outer @Requester t req' rsp'@ member of the effect row.

'mapAuthE' is a two-line composition of 'withQueryTE' and
'withRequesterTE' — the exact same shape as the MTL 'mapAuth', which
composes 'withQueryT' + 'withRequesterT' + 'runQueryT' + 'tellQueryDyn'.
The Effectful port doesn't need 'runQueryT' because 'withQueryTE'
handles the ambient plumbing (feeding the mapped 'Incremental' back
via 'tellQueryIncremental' internally), so 'mapAuthE' peels 'Query t q''
and 'Requester t (ApiRequest () pub priv) Identity' directly against
their outer siblings.

= What has landed since this docstring was first written

@reflex-dom-effectful@ now ships:

  * 'Reflex.Effectful.Internal.Channel.WORequesting' — the per-scope
    response-bridge channel-op. Runs ONCE per 'runRequesterEff'
    scope; the widget-loop allocates a 'newTriggerEvent' pair, the
    aggregator body ('runRequesterEff') consumes the trigger while
    it collects per-'Requesting' outbound events.
  * 'Reflex.Host.HeadlessEffectful.runRequesterEff' — Effectful
    sibling of MTL 'Reflex.Requester.Base.runRequesterT'. Peels
    'Requester t req rsp' via 'reinterpret' against a private
    'Effectful.State.Static.Local.State' collector, encodes each
    'Requesting' call's DMap-batch into a 'RequesterData req' via
    'Reflex.Requester.Base.Internal.singletonRequesterData' +
    'mergeRequesterData', merges the per-call outbound events into
    a single aggregated 'Event t (RequesterData req)' at scope close.

= Rhyolite runners — implemented

Both Rhyolite-shaped compositors are defined in this module:

  * 'runRhyoliteWidgetE' — Effectful sibling of the deleted MTL
    'Rhyolite.Frontend.App.runRhyoliteWidget'. Composes
    'Rhyolite.Frontend.App.openWebSocketE' +
    'Rhyolite.Frontend.Effect.Requester.matchResponsesWithRequestsE' +
    'Rhyolite.Frontend.Effect.Requester.runRequesterEff' +
    'runQueryTE' +
    'Reflex.Dynamic.holdUniqDyn' +
    'Rhyolite.Frontend.App.fromNotificationsE'.
    The recursive knot between the outbound request event, the inbound
    notification\/response streams, and the view-selector 'Dynamic' is
    closed with 'RecursiveDo' + the ambient @MonadFix (Eff es)@ instance.
  * 'runJengaRhyoliteWidgetE' — Effectful sibling of the deleted MTL
    'Rhyolite.Frontend.App.runJengaRhyoliteWidget'. Looks the
    WebSocket URL up via 'JCfg.getConfig' from
    "Jenga.Frontend.Effectful.Configs" and parses it with
    'parseURI'. Any config-lookup or URI-parse failure surfaces as
    @Left 'ObeliskRouteConfigError'@ rather than 'error' — per the
    codebase's \"no new 'error'\" rule and the \"bubble up errors\"
    principle. On success delegates to 'runRhyoliteWidgetE'.

The 'MonadHold' obligation of 'matchResponsesWithRequests' was resolved
by INLINING the body of 'matchResponseMapWithRequests' against
'EFH.hold' + 'EFH.holdIncremental' (see 'matchResponsesWithRequestsE'
below). The alternative — a 'MonadHold t (Eff es)' shim in
reflex-dom-effectful — was rejected because it would pollute every
consumer's effect row with an unbounded 'MonadHold' obligation whether
or not they use it, coupling the reflex-dom-effectful surface to the MTL
class hierarchy and undermining the whole point of the E-suffixed API.
The inline port is ~55 LOC, preserves exact semantics, and is the only
callsite that needed lifting.

= Bridge pattern

The honest interpret pattern used for the Query-side runners is:

@
runQueryTE eff qr =
  'reinterpret' setup handler eff
    where
      setup    = runReader qr . runState mempty
      handler AskQueryResult          = ask
      handler (TellQueryIncremental q) = modify (\\ (behs, evs) -> ...)
      handler (QueryIncremental q)    = do modify (...); crop \<$\> ask
@

The 'Query' effect is peeled and its state is threaded through private
'Reader' + 'State' effects that live only for the duration of the
interpretation. At the outer boundary the accumulated state is
converted back to the 'Incremental' the MTL runner promises. No
IORefs, no per-op IO extraction, no state loss — the collector
survives across every operation because it lives on the Eff stack.

For the Requester-side bridge ('withRequesterTE'), the pattern is a
straight pass-through interpretation — no private state is needed
because the outer 'Requester t req' rsp'' handles all the routing.
The handler simply maps the outbound 'DMap' with @DMap.map freq@
under the event functor, delegates to 'requesting'\/'requesting_' on
the outer effect, and maps the inbound 'DMap' back with @DMap.map frsp@.
-}
module Rhyolite.Frontend.Effect.WidgetRunner
  ( -- * Honest ports (implemented)
    runQueryTE
  , withQueryTE
  , withRequesterTE
  , mapAuthE
    -- * Rhyolite widget compositors
  , runRhyoliteWidgetE
  , runJengaRhyoliteWidgetE
    -- * Route-config errors surfaced by 'runJengaRhyoliteWidgetE'
  , ObeliskRouteConfigError (..)
  ) where

-- Note on re-exports: we intentionally do NOT re-export
-- "Reflex.Host.HeadlessEffectful" from here. The 'Query' effect data
-- type shares a name with the 'Reflex.Query.Class.Query' typeclass
-- (its 'MonadQuery' superclass), and unqualified co-existence in a
-- single module's namespace triggers a name-collision. Consumers of
-- 'runQueryTE' / 'withQueryTE' should import
-- "Reflex.Host.HeadlessEffectful" directly (qualified as they see fit)
-- for the underlying effect operations.

import           Data.Foldable                       (foldlM)
import           Data.Functor.Identity               (Identity(..))
import           Effectful                           (Eff, (:>))
import           Effectful.Dispatch.Dynamic          (LocalEnv, interpret,
                                                      reinterpret)
import qualified Effectful.Reader.Static             as EFR
import qualified Effectful.State.Static.Local        as EFST

import           Data.Patch                          (AdditivePatch(..), Group)
import           Data.Semigroup.Commutative          (Commutative)

import           Reflex.Class              (Behavior, Dynamic, Event,
                                                      Incremental, Reflex,
                                                      current, fmapCheap,
                                                      incrementalToDynamic,
                                                      mergeWith, never,
                                                      sample, unsafeBuildIncremental,
                                                      zipDynWith)
import qualified Reflex.Class              as R
import qualified Reflex.Query.Class  as RQC
import           Reflex.Query.Class  (QueryMorphism(..),
                                                      QueryResult, crop, mapQuery,
                                                      mapQueryResult)

-- (Removed: Reflex.Effectful.Core.Timeline / Core.Class / Types.
-- Data-family wrappers EffIncremental/and the KnownTimeline
-- singleton are gone — the port uses bare Reflex.Class Incremental/Dynamic
-- and carries `t` polymorphically without needing an EffTimeline pin.)
-- NB: 'WidgetChannel' is intentionally NOT imported here. Earlier drafts of
-- 'runRhyoliteWidgetE' / 'runObeliskRhyoliteWidgetE' carried a
-- 'WidgetChannel t :> es' constraint out of caution because the interpreters
-- for the reflex-dom-effectful primitives ('Hold', 'PostBuild', 'PerformEvent',
-- 'TriggerEvent', 'JSM'', 'Prerender', 'Adjustable') are themselves defined
-- against 'WidgetChannel'. That constraint was vestigial at this layer: this
-- module never sends a 'WidgetOp' directly and never invokes 'sendReq'; it
-- only consumes the *already-installed* effect memberships. The runners
-- listed above discharge 'WidgetChannel' internally when the app root
-- (typically 'mainWidgetEff' / 'JFS.runFrontend') interprets them. Callers
-- of 'runRhyoliteWidgetE' who sit inside such a root inherit each of those
-- effect memberships without ever needing to see 'WidgetChannel' themselves
-- — dropping the constraint here unblocks the frontend body slot, whose
-- 'WidgetEff' constraint bag provides everything except 'WidgetChannel'.

-- Bring the Query effect's operations into scope for building the
-- handler and for 'withQueryTE''s outer 'askQueryResult' /
-- 'tellQueryIncremental' calls. The constructors
-- ('AskQueryResult', 'TellQueryIncremental', 'QueryIncremental') are
-- referenced through the 'EFQ' qualifier in the handler pattern-match
-- so we don't risk shadowing 'RQC.Query' (the typeclass) with 'EFQ.Query'
-- (the effect data type) in the unqualified namespace.
import           Reflex.Host.HeadlessEffectful
                    ( askQueryResultE
                    , tellQueryIncrementalE
                    )
import qualified Reflex.Host.HeadlessEffectful       as EFQ

-- Requester effect: primitive ops and the effect data type. The 'Requesting'
-- and 'Requesting_' constructors are pattern-matched against in the
-- 'withRequesterTE' handler. We qualify as 'EFRQ' to keep names distinct
-- from any MTL 'Reflex.Requester.*' import.
import qualified Reflex.Host.HeadlessEffectful   as EFRQ
import           Reflex.Host.HeadlessEffectful   (requestingE, requestingE_)

-- ApiRequest constructors used by 'mapAuthE' to re-authorize public /
-- private requests emitted by the inner (unauthenticated) widget. Imported
-- from Rhyolite's public API module — same import the MTL 'mapAuth' uses.
import           Rhyolite.Api                        (ApiRequest(..))

-- =============================================================================
-- Additional imports used only by 'runRhyoliteWidgetE' /
-- 'runJengaRhyoliteWidgetE' below.
--
-- Kept as a separate import block from the ones above so it's visible at
-- a glance which imports belong to the honest bridges ('runQueryTE' etc.)
-- vs which belong to the Rhyolite compositors.
-- =============================================================================

-- Bifunctor for the '(notification, response)' split of the
-- 'Dynamic t (AppWebSocket t qWire)' produced by 'prerender'.
import           Data.Bifunctor                      (bimap)

-- Map helpers used to filter\/re-tag outbound wire requests.
import qualified Data.Map                            as Map

-- Text encoding for reading the config-file bytes as a URL string.
-- 'ByteString' is imported qualified to disambiguate from the strict-lazy
-- pair; only strict is needed because 'JCfg.getConfig' returns the strict
-- variant.
import           Data.ByteString                     (ByteString)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE

-- Aeson: encoding of the outbound wire request; parsing of the (opaque)
-- wire response payload in 'reqEncoder'.
import qualified Data.Aeson                          as Aeson
import           Data.Aeson                          (FromJSON, ToJSON,
                                                      Result(Success),
                                                      Value)

-- Constraint plumbing used by 'reqEncoder'. Direct port of the MTL
-- 'runRhyoliteWidget' body: 'whichever' witnesses @ToJSON (req a)@ from
-- the @ForallF ToJSON req@ side of 'Rhyolite.Api.Request'; 'has'
-- witnesses @FromJSON a@ from the @Has FromJSON req@ side.
import           Data.Constraint.Extras              (has, whichever)
import           Control.Monad                       (join)

-- 'Some' packaging of the wire-decoded request. Matches MTL body's
-- @Some req@ intermediary between 'fromJSON' and 'TaggedRequest'.
import           Data.Some                           (Some)

-- Network URI parsing for the Jenga compositor. 'websocketUri' is
-- imported below from Rhyolite.WebSocket alongside the tagged wire
-- types.
import           Network.URI                         (URI, parseURI)

-- Effectful primitives the reflex bindings live on.
import qualified Effectful                           as EFF

-- Reflex-effectful ambient effects the compositors need at various
-- points. 'EFH.Hold' feeds both the response-matcher and 'holdUniqDyn';
-- the others feed the 'openWebSocketE' / 'fromNotificationsE' calls.
import qualified Reflex.Host.HeadlessEffectful        as EFH
import qualified Reflex.Host.HeadlessEffectful   as EFPB
import qualified Reflex.Host.HeadlessEffectful as EFPE
import qualified Reflex.Host.HeadlessEffectful as EFTE
import qualified Reflex.Host.HeadlessEffectful      as EFS
import qualified Reflex.Host.Class                  as RH
import           Control.Monad.IO.Class             (MonadIO)
import qualified Reflex.Effectful.DomHost as EFJSM

-- Prerender: the compositor runs 'openWebSocketE' and 'fromNotificationsE'
-- inside a client-only 'prerender' branch (matching the MTL 'prerender'
-- calls). 'HasPrerender t space es' means the server branch is discarded
-- and the client branch runs in the same effect row as the outer;
-- 'space' is a scoped type variable pinning the client-side DOM space.
import qualified Reflex.Effectful.DomHost      as EPr

-- DomBuilder / DomBuilderSpace: the reshaped 'HasPrerender' class head
-- carries the DOM-space parameter as a scoped type variable, so callers
-- name a matching 'DomBuilder t space :> es' constraint alongside the
-- prerender constraint.
import           Reflex.Effectful.DomHost  (DomBuilder)

-- 'holdUniqDyn' — nubs the view-selector 'Dynamic' before it goes on
-- the wire so identical successive view-selector snapshots don't
-- retrigger a WebSocket send.
import qualified Reflex.Dynamic            as EFDyn

-- Class-level pure reflex combinators used in the compositor body.
-- Matches the MTL 'switch . current . splitDynPure' / 'fmapMaybe' /
-- 'ffor' / 'leftmost' calls line-for-line. 'splitDynPure' lives in
-- "Reflex.Dynamic" (not re-exported by
-- "Reflex.Class"), imported separately below.
import           Reflex.Class              (ffor, fmapMaybe, switch)
import           Reflex.Dynamic            (splitDynPure)

-- E-side aggregator that batches outbound 'requesting' calls into
-- 'RequesterData req' (and feeds the correlated response back through).
-- Ported in Phase 1 alongside 'matchResponsesWithRequestsE'.
import qualified Rhyolite.Frontend.Effect.Requester  as EFRhyReq

-- WebSocket wire types the compositor threads through the
-- 'openWebSocketE' + 'matchResponsesWithRequestsE' loop.
import           Rhyolite.WebSocket                  (TaggedRequest(..),
                                                      TaggedResponse(..),
                                                      websocketUri)

-- Request typeclass bag used to constrain 'req' in 'runRhyoliteWidgetE'.
-- Same shape as the MTL sibling: 'Rhyolite.Api.Request' is
-- @(ForallF ToJSON r, Has ToJSON r, FromJSON (Some r), Has FromJSON r)@.
import           Rhyolite.Api                        (Request)

-- Rhyolite frontend surface: the WebSocket wrapper record, the
-- Effectful WebSocket opener, the streaming-view-of-notifications
-- combinator. These are the honest E-side ports Phase 1 landed.
import           Rhyolite.Frontend.App               (AppWebSocket(..))
import           Rhyolite.Frontend.App.Effectful     ( fromNotificationsE
                                                      , openWebSocketE)

-- E-side 'RequesterData' tag machinery: same shape as MTL's copy;
-- 'traverseRequesterData' is the sieve that lifts a per-field decoder
-- ('Maybe (Identity a)') into a whole-batch decoder
-- ('Maybe (RequesterData Identity)') and drops the batch entirely if
-- any single field failed to decode.
import           Reflex.Requester.Base     (Entry(..), Multi, Multi3, RequesterData, traverseRequesterData)

-- Jenga's app-config effect. 'runJengaRhyoliteWidgetE' looks the
-- WebSocket base URL up here.
import qualified Jenga.Frontend.Effectful.Configs    as JCfg

-- Jenga route surface: 'FullRoute' shape, the checked 'Encoder' the
-- caller supplies, the URL-renderer for the concrete "listen" route,
-- and 'R' (indexed-DSum) route synonym.
import           Jenga.Route                         (Encoder, FullRoute,
                                                      PageName,
                                                      renderObeliskBackendRoute)
import           Jenga.Route.Core                    (R)


-- -----------------------------------------------------------------------------
-- 'runQueryTE'
--
-- Effectful sibling of 'Reflex.Query.Base.runQueryT':
--
-- @
-- runQueryT
--   :: (MonadFix m, Commutative q, Group q, Reflex t)
--   => QueryT t q m a
--   -> Dynamic t (QueryResult q)
--   -> m (a, Incremental t (AdditivePatch q))
-- @
--
-- The Effectful port matches the shape verbatim, except the input is
-- an 'Eff (Query t q ': es) a' and the output lives in 'Eff es'. The
-- ambient 'MonadFix' is dischargeable because 'Eff es' has a global
-- 'Control.Monad.Fix.MonadFix' instance.
--
-- The internal collector accumulates:
--
--   * 'behs'  :: '[Behavior t q]'   — the "current" side, folded with
--     'sample' + 'mappend' into the 'Incremental''s pull-fn (matches
--     'foldlM' inside the MTL runner).
--   * 'evs'   :: '[Event t q]'      — the "updates" side, merged with
--     'mergeWith mappend' and wrapped in 'AdditivePatch' (matches
--     'fmapCheap AdditivePatch' inside the MTL runner).
--
-- No throttling, no debounce, no reordering: we merge in the order
-- 'tellQueryIncremental' was called, exactly as the MTL sibling does
-- via its 'StateT [Behavior t q]' \/ 'EventWriterT' collector.
-- -----------------------------------------------------------------------------

-- | Internal collector state for 'runQueryTE'. Mirrors the pair of
-- writer states baked into 'Reflex.Query.Base.QueryT'
-- ('StateT [Behavior t q]' and the 'EventWriterT' side).
data QueryCollector t q = QueryCollector
  { _queryCollector_behaviors :: ![Behavior t q]
    -- ^ Prepended in reverse order (matches MTL's 'modify (currentIncremental q:)').
  , _queryCollector_events    :: ![Event t q]
    -- ^ Prepended in reverse order (matches MTL's 'EventWriterState' collector).
  }

-- | Constraint alias for the effect requirements 'runQueryTE' imposes
-- on the outer effect row. Kept tight: only the effects the collector
-- itself needs, plus what any consumer must already carry.
--
-- Note: 'RQC.Query' here is the @MonadQuery@ superclass (the
-- typeclass) from "Reflex.Query.Class", NOT the @Query@ effect data
-- type from "Reflex.Host.HeadlessEffectful" (which we import unqualified
-- because it's the effect being interpreted). These two shadow each
-- other by name, hence the qualified reference.
type RunQueryEff t q (es :: [EFF.Effect]) =
  ( Reflex t
  , Group q
  , Commutative q
  , RQC.Query q
  )

-- | Effectful port of 'Reflex.Query.Base.runQueryT'.
--
-- Runs an @Eff (Query t q ': es) a@ against a supplied ambient
-- @Dynamic t (QueryResult q)@ and returns the same @(a, Incremental t
-- (AdditivePatch q))@ that the MTL runner produces. See the module
-- docstring for the bridge pattern.
runQueryTE
  :: forall t q (es :: [EFF.Effect]) a.
     RunQueryEff t q es
  => Eff (EFQ.Query t q ': es) a
  -> Dynamic t (QueryResult q)
  -> Eff es (a, Incremental t (AdditivePatch q))
runQueryTE eff qr = do
  (a, QueryCollector behs evs) <-
    -- Reinterpret the Query effect against a private Reader (holding
    -- the ambient QueryResult) + State (holding the collector). Both
    -- effects are scoped strictly to this call; nothing leaks out.
    reinterpret
      (\action ->
         EFR.runReader qr $
           EFST.runState (QueryCollector [] []) action)
      queryHandler
      eff
  let (behsInOrder, evsInOrder) = (reverse behs, reverse evs)
      -- Pull-fn: sample every collected behaviour and mappend them,
      -- exactly like the MTL runner:
      --   foldlM (\b c -> (b <>) <$> sample c) mempty bs
      pullFn = foldlM (\acc b -> (acc <>) <$>
                                    sample b) (mempty :: q) behsInOrder
      -- Updates event: merge every collected event with mappend, then
      -- lift into AdditivePatch. MTL uses fmapCheap AdditivePatch on
      -- an EventWriterT-merged single event; mergeWith mappend
      -- produces the same semantic event (all firings folded via <>).
      updatesEv = case evsInOrder of
        [] -> never
        _  -> fmapCheap AdditivePatch (mergeWith (<>) evsInOrder)
  pure (a, unsafeBuildIncremental pullFn updatesEv)
  where
    -- The handler runs in the reinterpreted stack:
    -- 'EFST.State (QueryCollector t q) : EFR.Reader (Dynamic t (QueryResult q)) : es'.
    -- Reader/State access is discharged by concrete membership in the
    -- return Eff row; no ':> localEs' constraints — EffectHandler's
    -- polymorphic localEs only carries 'EFQ.Query t q :> localEs'.
    queryHandler
      :: forall localEs r.
         LocalEnv
           localEs
           (EFST.State (QueryCollector t q)
              ': EFR.Reader (Dynamic t (QueryResult q))
              ': es)
      -> EFQ.Query t q (Eff localEs) r
      -> Eff (EFST.State (QueryCollector t q)
                ': EFR.Reader (Dynamic t (QueryResult q))
                ': es) r
    queryHandler _env = \case
      -- MTL: askQueryResult = QueryT ask
      EFQ.AskQueryResult ->
        EFR.ask
      -- MTL:
      --   tellQueryIncremental q = do
      --     QueryT (modify (currentIncremental q:))
      --     QueryT (lift (tellEvent (fmapCheap unAdditivePatch (updatedIncremental q))))
      EFQ.TellQueryIncremental primInc -> do
        let inc = primInc :: Incremental t (AdditivePatch q)
            beh = R.currentIncremental inc
            ev  = fmapCheap unAdditivePatch (R.updatedIncremental inc)
        EFST.modify (\ (QueryCollector bs es) ->
                       QueryCollector (beh : bs) (ev : es))
      -- MTL:
      --   queryIncremental q = do
      --     tellQueryIncremental q
      --     zipDynWith crop (incrementalToDynamic q) <$> askQueryResult
      EFQ.QueryIncremental primInc -> do
        let inc = primInc :: Incremental t (AdditivePatch q)
            beh = R.currentIncremental inc
            ev  = fmapCheap unAdditivePatch (R.updatedIncremental inc)
        EFST.modify (\ (QueryCollector bs es) ->
                       QueryCollector (beh : bs) (ev : es))
        qrDyn <- EFR.ask
        pure $ zipDynWith crop (R.incrementalToDynamic inc) qrDyn

-- -----------------------------------------------------------------------------
-- 'withQueryTE'
--
-- Effectful sibling of 'Reflex.Query.Base.withQueryT':
--
-- @
-- withQueryT
--   :: (MonadFix m, PostBuild t m, Group q, Group q', Commutative q, Commutative q', Query q')
--   => QueryMorphism q q'
--   -> QueryT t q m a
--   -> QueryT t q' m a
-- withQueryT f a = do
--   r' <- askQueryResult
--   (result, q) <- lift $ runQueryT a $ mapQueryResult f <$> r'
--   tellQueryIncremental $ unsafeBuildIncremental
--     (fmap (mapQuery f) (sample (currentIncremental q)))
--     (fmapCheap (AdditivePatch . mapQuery f . unAdditivePatch) $ updatedIncremental q)
--   return result
-- @
--
-- The Effectful body is a verbatim structural port: ask the outer
-- 'Query t q'' for its ambient result, transport it through the
-- 'QueryMorphism' to produce the inner ambient result, run the inner
-- @Eff (Query t q ': es)@ against it via 'runQueryTE', then re-tell
-- the (mapped) incremental back into the outer 'Query t q'' collector.
-- -----------------------------------------------------------------------------

-- | Effectful port of 'Reflex.Query.Base.withQueryT'. Same
-- 'QueryMorphism' semantics as the MTL sibling.
withQueryTE
  :: forall t q q' es a.
     ( Reflex t
     , Group q, Commutative q, RQC.Query q
     , Group q', Commutative q', RQC.Query q'
     , EFQ.Query t q' :> es
     )
  => QueryMorphism q q'
  -> Eff (EFQ.Query t q ': es) a
  -> Eff es a
withQueryTE f eff = do
  -- MTL: r' <- askQueryResult   (from the outer QueryT t q')
  -- 'askQueryResult' returns 'CoreT.Dynamic Core.Global (QueryResult q')';
  -- wrap in 'EffDynamic' to get the class-level 'Dynamic t' that 'runQueryTE' expects.
  r' <- askQueryResultE @t @q'
  -- MTL: (result, q) <- lift $ runQueryT a $ mapQueryResult f <$> r'
  let rDyn :: Dynamic t (QueryResult q)
      rDyn = fmap (mapQueryResult f) (r')
  (result, incQ) <- runQueryTE @t @q eff rDyn
  -- MTL:
  --   tellQueryIncremental $ unsafeBuildIncremental
  --     (fmap (mapQuery f) (sample (currentIncremental q)))
  --     (fmapCheap (AdditivePatch . mapQuery f . unAdditivePatch) $
  --        updatedIncremental q)
  let mappedIncremental :: Incremental t (AdditivePatch q')
      mappedIncremental =
        unsafeBuildIncremental
          (fmap (mapQuery f) (sample (R.currentIncremental incQ)))
          (fmapCheap
             (AdditivePatch . mapQuery f . unAdditivePatch)
             (R.updatedIncremental incQ))
  -- 'tellQueryIncremental' takes 'PrimInc.Incremental Core.Global'; unwrap
  -- the class-level newtype to hand it over.
  tellQueryIncrementalE @t @q' (id mappedIncremental)
  pure result

-- -----------------------------------------------------------------------------
-- 'withRequesterTE'
--
-- Effectful sibling of 'Reflex.Requester.Base.withRequesterT':
--
-- @
-- withRequesterT
--   :: (Reflex t, MonadFix m)
--   => (forall x. req x -> req' x)   -- request mapping (inner -> outer)
--   -> (forall x. rsp' x -> rsp x)   -- response mapping (outer -> inner)
--   -> RequesterT t req rsp m a      -- inner child
--   -> RequesterT t req' rsp' m a    -- outer widget
-- @
--
-- Effectful port peels the inner @Requester t req rsp@ effect layer and
-- delegates scalar and bundled calls to the outer
-- @Requester t req' rsp'@ member of the effect row. Payload mappings
-- preserve the bundle shape and all request tags and cohort keys.
--
-- The pinned 'Reflex.Host.HeadlessEffectful' Requester carries scalar
-- payloads or 'Entry' bundles. 'traverseRequesterData' maps the nested
-- payloads without allocating tags or changing response routing.
-- -----------------------------------------------------------------------------

-- | Effectful port of 'Reflex.Requester.Base.withRequesterT'.
--
-- Reinterprets an @Eff (Requester t req rsp ': es) a@ against an outer
-- @Requester t req' rsp'@ effect that lives in @es@, mapping requests
-- through @freq :: forall x. req x -> req' x@ and responses back through
-- @frsp :: forall x. rsp' x -> rsp x@. Semantics match the MTL sibling
-- verbatim: every 'Requesting' call fired inside the inner effect layer
-- becomes exactly one 'Requesting' call on the outer effect layer with
-- the mapped payloads.
withRequesterTE
  :: forall t req req' rsp rsp' es a.
     ( Reflex t
     , EFRQ.Requester t req' rsp' :> es
     )
  => (forall x. req x -> req' x)
     -- ^ Map inner requests (@req@) to outer requests (@req'@).
  -> (forall x. rsp' x -> rsp x)
     -- ^ Map outer responses (@rsp'@) back to inner responses (@rsp@).
  -> Eff (EFRQ.Requester t req rsp ': es) a
  -> Eff es a
withRequesterTE freq frsp = interpret handler
  where
    -- The handler runs in the ORIGINAL 'es' effect row (that's what
    -- 'interpret' hands us — no reinterpret, no private state), so we
    -- can call 'requesting' / 'requesting_' on the outer Requester
    -- directly. Each branch maps outbound payloads, delegates the same
    -- operation, and maps response payloads back when present.
    -- No ':> localEs' constraints: EffectHandler's polymorphic localEs
    -- only carries 'Requester t req rsp :> localEs' (the effect being
    -- interpreted). Access to the outer 'Requester t req' rsp'' happens
    -- in the return 'Eff es r' and is discharged by the enclosing
    -- signature's 'EFRQ.Requester t req' rsp' :> es'.
    handler
      :: forall localEs r.
         LocalEnv localEs es
      -> EFRQ.Requester t req rsp (Eff localEs) r
      -> Eff es r
    handler _env = \case
      EFRQ.Requesting evReq -> do
        -- Map inner request payload to outer payload:
        --   evReq   :: Event t (req a)
        --   evReq'  :: Event t (req' a)
        let evReq' = fmap freq evReq
        -- Delegate to outer Requester. Returns an Event t (rsp' a).
        evRsp' <- requestingE @t @req' @rsp' evReq'
        -- Map outer response payload back to inner payload:
        --   evRsp'  :: Event t (rsp' a)
        --   result  :: Event t (rsp a)
        pure (fmap frsp evRsp')
      EFRQ.Requesting_ evReq -> do
        let evReq' = fmap freq evReq
        requestingE_ @t @req' @rsp' evReq'
      EFRQ.RequestingBundled evReq -> do
        evRsp <- EFRQ.requestingBundledE @t @req' @rsp' (mapRequesterMulti freq <$> evReq)
        pure (mapRequesterMulti frsp <$> evRsp)
      EFRQ.RequestingBundled3 evReq -> do
        evRsp <- EFRQ.requestingBundled3E @t @req' @rsp' (mapRequesterMulti3 freq <$> evReq)
        pure (mapRequesterMulti3 frsp <$> evRsp)

-- Preserve request tags and cohort keys while translating payloads.
mapRequesterPayload
  :: (forall x. req x -> rsp x)
  -> RequesterData req -> RequesterData rsp
mapRequesterPayload f = runIdentity . traverseRequesterData (Identity . f)

mapRequesterMulti
  :: (forall x. req x -> rsp x)
  -> Entry req Multi -> Entry rsp Multi
mapRequesterMulti f (Entry batches) = Entry (fmap (mapRequesterPayload f) batches)

mapRequesterMulti3
  :: (forall x. req x -> rsp x)
  -> Entry req Multi3 -> Entry rsp Multi3
mapRequesterMulti3 f (Entry batches) = Entry (fmap (fmap (mapRequesterPayload f)) batches)

-- -----------------------------------------------------------------------------
-- 'mapAuthE'
--
-- Effectful sibling of 'Rhyolite.Frontend.App.mapAuth':
--
-- @
-- mapAuth
--   :: forall pub priv q q' a cred t m.
--      ( MonadFix m, PostBuild t m
--      , Query q, Group q, Commutative q, Group q', Commutative q'
--      )
--   => cred
--   -> QueryMorphism q' q
--   -> QueryT t q' ((RequesterT t (ApiRequest () pub priv)) Identity m) a
--   -> RhyoliteWidget q (ApiRequest cred pub priv) t m a
-- mapAuth token authorizeQuery authenticatedChild = RhyoliteWidget $ do
--   v <- askQueryResult
--   (a, vs) <- lift $ withRequesterT authorizeReq id $
--     runQueryT (withQueryT authorizeQuery authenticatedChild) v
--   tellQueryDyn $ incrementalToDynamic vs
--   return a
--   where
--     authorizeReq :: forall x. ApiRequest () pub priv x
--                             -> ApiRequest cred pub priv x
--     authorizeReq = \\case
--       ApiRequest_Public a       -> ApiRequest_Public a
--       ApiRequest_Private () a   -> ApiRequest_Private token a
-- @
--
-- The Effectful port sheds the MTL scaffolding: no 'RhyoliteWidget'
-- newtype, no manual 'runQueryT' + 'tellQueryDyn' round-trip (that's
-- exactly what 'withQueryTE' handles internally), no 'MonadFix'\/
-- 'PostBuild' plumbing at the site (both are subsumed by the effect
-- constraints on the outer 'Query' \/ 'Requester' handlers). The body
-- collapses to the two peel operations, composed:
--
--   1. Peel 'Query t q'' against outer 'Query t q' via 'withQueryTE'
--      (which handles the incremental round-trip that MTL 'mapAuth'
--      does with @runQueryT@ + @tellQueryDyn@).
--   2. Peel 'Requester t (ApiRequest () pub priv) Identity' against
--      outer 'Requester t (ApiRequest cred pub priv) Identity' via
--      'withRequesterTE', using the same 'authorizeReq' request map
--      and 'id' response map the MTL sibling uses.
-- -----------------------------------------------------------------------------

-- | Effectful port of 'Rhyolite.Frontend.App.mapAuth'.
--
-- Peels the inner @Query t q'@ and @Requester t (ApiRequest () pub priv)
-- Identity@ effects and re-emits them on the outer @Query t q@ /
-- @Requester t (ApiRequest cred pub priv) Identity@ layers, authorizing
-- each private request with the supplied @cred@ token and transporting
-- queries through the supplied 'QueryMorphism'. Semantics match the
-- MTL sibling verbatim (same @authorizeReq@, same query transport).
mapAuthE
  :: forall t q q' pub priv cred es a.
     ( Reflex t
     , Group q, Commutative q, RQC.Query q
     , Group q', Commutative q', RQC.Query q'
     , EFQ.Query t q :> es
     , EFRQ.Requester t (ApiRequest cred pub priv) Identity :> es
     )
  => cred
     -- ^ Authentication token. Used to promote every
     -- @ApiRequest_Private ()@ from the inner (unauthenticated) widget
     -- to an @ApiRequest_Private cred@ on the outer wire.
  -> QueryMorphism q' q
     -- ^ Transport queries emitted by the inner widget (in @q'@) up
     -- to the outer query type @q@. Same morphism argument the MTL
     -- 'mapAuth' takes.
  -> Eff (EFQ.Query t q' ': EFRQ.Requester t (ApiRequest () pub priv) Identity ': es) a
     -- ^ The inner (unauthenticated) child widget effect stack.
  -> Eff es a
mapAuthE token authorizeQuery authenticatedChild =
  -- Peel Requester first (outermost in the effect list = outermost peel).
  -- Then peel Query. The order matches the MTL body's nesting:
  --   withRequesterT authorizeReq id $ runQueryT (withQueryT ...) v
  -- (withQueryT is inside withRequesterT because 'runQueryT' hands its
  -- result up through withRequesterT's lift.)
  withRequesterTE @t @(ApiRequest () pub priv) @(ApiRequest cred pub priv)
                  @Identity @Identity
                  authorizeReq id
    (withQueryTE @t @q' @q authorizeQuery authenticatedChild)
  where
    -- Same authorization function the MTL 'mapAuth' uses. The @x@ index
    -- is the request-response payload type index; 'ApiRequest' preserves
    -- it through both constructors, so the mapping is index-invariant.
    authorizeReq
      :: forall x. ApiRequest () pub priv x
                -> ApiRequest cred pub priv x
    authorizeReq = \case
      ApiRequest_Public a     -> ApiRequest_Public a
      ApiRequest_Private () a -> ApiRequest_Private token a


-- -----------------------------------------------------------------------------
-- 'ObeliskRouteConfigError'
--
-- Failure surface of 'runJengaRhyoliteWidgetE'. The MTL sibling
-- 'runJengaRhyoliteWidget' produces the equivalent of these two conditions
-- with 'error':
--
-- @
--   let route = case r' of
--         Nothing        -> error $ ... "route config missing: " <> configRoute
--         Just Nothing   -> error $ ... "malformed confing route: " <> configRoute
--         Just (Just r)  -> r
-- @
--
-- The E-side compositor bubbles the two states out as a 'Left' constructor
-- so consumers can render a friendlier message, retry, fall back to a
-- default URL, etc. — none of which are decisions this library gets to
-- make on their behalf (see the module-header docstring, and the
-- codebase's "Bubble up errors — never bury them in the wrapper" rule).
-- -----------------------------------------------------------------------------

-- | Errors surfaced by 'runJengaRhyoliteWidgetE' when resolving the
-- WebSocket base URL from the frontend's config directory.
data ObeliskRouteConfigError
  = ObeliskRouteConfigMissing !Text
    -- ^ The config key was not present in the config map at all. The
    -- attached 'Text' is the requested key so the caller can point the
    -- user at the exact file that's expected.
  | ObeliskRouteConfigMalformed !Text
    -- ^ The config key was present but the byte payload did not parse
    -- as a URI via 'Network.URI.parseURI'. The attached 'Text' is the
    -- config key (not the malformed bytes — those may be secrets and
    -- are the caller's concern to surface at their discretion).
  deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- 'runRhyoliteWidgetE'
--
-- Effectful sibling of the deleted MTL
-- 'Rhyolite.Frontend.App.runRhyoliteWidget'.
--
-- The MTL body:
--
-- @
-- runRhyoliteWidget toWire url child = do
--   let defAppWebSocket = ...
--   rec dAppWebSocket <- prerender (pure defAppWebSocket) $
--         openWebSocket url request'' nubbedVs
--       let (notification, response) =
--             bimap (switch . current) (switch . current) . splitDynPure $
--               ffor dAppWebSocket $ \\ws ->
--                 (_appWebSocket_notification ws, _appWebSocket_response ws)
--       (request', response') <- matchResponsesWithRequests reqEncoder request $
--         ffor response $ \\(TaggedResponse t v) -> (t, v)
--       let request'' = fmap (Map.elems . Map.mapMaybeWithKey (\\t v -> ...)) request'
--       ((a, vs), request) <- flip runRequesterT
--           (fmapMaybe (traverseRequesterData (fmap Identity)) response') $
--         runQueryT (unRhyoliteWidget child) view
--       let vsDyn = incrementalToDynamic vs
--       nubbedVs <- holdUniqDyn (mapQuery toWire <$> vsDyn)
--       view <- fmap join $ prerender (pure mempty) $
--         fromNotifications vsDyn $ mapQueryResult toWire <$> notification
--   return (dAppWebSocket, a)
-- @
--
-- ports here as a structural-shape-for-shape rewrite:
--
--   * 'runRequesterT' + 'runQueryT' composed on the MTL child →
--     'EFRhyReq.runRequesterEff' (outer peel, Requester) composed with
--     'runQueryTE' (inner peel, Query) on the effectful child.
--   * 'matchResponsesWithRequests' →
--     'EFRhyReq.matchResponsesWithRequestsE'.
--   * 'holdUniqDyn' → 'EFDyn.holdUniqDyn'
--     (from "Reflex.Dynamic").
--   * 'prerender' → 'EPr.prerender' (server branch discarded; client
--     branch runs in the outer effect row via 'HasPrerender t space es').
--   * 'openWebSocket' / 'fromNotifications' → their E-side siblings from
--     "Rhyolite.Frontend.App".
--   * MTL 'error' paths — none in this compositor; the URL is a plain
--     'Text' argument.
--
-- Named helpers ('defAppWebSocket', 'splitNotifResponse',
-- 'decodeRequestBatch', 'buildViewDyn') factor the body so each
-- sub-computation is ~10 lines and the 'rec' knot in the main body
-- stays readable — same decomposition strategy the rest of this module
-- follows for 'runQueryTE' \/ 'withQueryTE'. The per-request encoder is
-- kept in the where-clause (see @reqEncoder@ inside 'runRhyoliteWidgetE')
-- because its rank-2 @forall a@ signature composes most cleanly with
-- the outer 'Request req' constraint at the binding site.
-- -----------------------------------------------------------------------------

-- | Constraint bag imposed by 'runRhyoliteWidgetE' on the outer effect row.
--
-- Named so the type of 'runRhyoliteWidgetE' fits on a screen. Kept in
-- one place so 'runJengaRhyoliteWidgetE' can reuse it verbatim (it only
-- adds the config-lookup obligation to the bag).
--
-- Note: 'RQC.Query' here is the @MonadQuery@ superclass (the typeclass)
-- from "Reflex.Query.Class", NOT the @Query@ effect data type from
-- "Reflex.Host.HeadlessEffectful" — same shadowing convention as
-- 'RunQueryEff' above.
type RunRhyoliteWidgetEC qFrontend qWire req t space es =
  ( Reflex t
  , EFH.Hold t :> es
  , EFS.Sample t :> es
  , EFPB.PostBuild t :> es
  , EFPE.PerformEventH t :> es
  , EFTE.TriggerEvent t :> es
  , EFJSM.JSM' :> es
  , EFF.IOE :> es
  , DomBuilder t space :> es
  , EPr.HasPrerender t space es
  , EFJSM.DomBuilderTimelineE (EPr.ClientEs t space es) ~ t
  , EFJSM.JSM' :> EPr.ClientEs t space es
  , EFF.IOE :> EPr.ClientEs t space es
  , EFPE.PerformEventM (RH.HostFrame t) t :> EPr.ClientEs t space es
  , EFPE.PerformableForRow (EPr.ClientEs t space es) ~ RH.HostFrame t
  , RH.ReflexHost t
  , MonadIO (RH.HostFrame t)
  , EFTE.TriggerEvent t :> EPr.ClientEs t space es
  , EFH.Hold t :> EPr.ClientEs t space es
  , EFH.Sample t :> EPr.ClientEs t space es
  , EFPB.PostBuild t :> EPr.ClientEs t space es
  , EFPE.PerformEventH t :> EPr.ClientEs t space es
  , Request req
  , RQC.Query qFrontend
  , Group qFrontend
  , Commutative qFrontend
  , Eq qWire
  , Monoid (QueryResult qFrontend)
  , FromJSON (QueryResult qWire)
  , ToJSON qWire
  )

-- | Effectful port of the deleted MTL
-- 'Rhyolite.Frontend.App.runRhyoliteWidget'.
--
-- Opens a WebSocket connection at @url@ (via 'openWebSocketE' inside a
-- 'prerender'), then runs the caller-supplied child widget as
-- @Eff (Query t qFrontend : Requester t req Identity : es) b@ against
-- (a) the ambient view-selector-result 'Dynamic' fed back from the
-- socket's notification channel, and (b) the response 'Event' correlated
-- against the child's outgoing requests via 'matchResponsesWithRequestsE'.
--
-- Returns the same @('Dynamic' t ('AppWebSocket' t qWire), b)@ pair the
-- MTL sibling returned — the socket handle stays available for consumers
-- who need to observe connection state, and the child's return value @b@
-- is passed through unchanged.
runRhyoliteWidgetE
  :: forall qFrontend qWire req es t space b.
     ( RunRhyoliteWidgetEC qFrontend qWire req t space es
     )
  => QueryMorphism qFrontend qWire
     -- ^ Wire-format morphism: transports view-selectors from the
     -- richer frontend currency ('qFrontend') down to the wire currency
     -- ('qWire') the backend serves. Same argument the MTL sibling took.
  -> Text
     -- ^ WebSocket URL (typically produced by 'renderObeliskBackendRoute'
     -- against the backend "listen" route; 'runJengaRhyoliteWidgetE'
     -- below does this construction for the Jenga case).
  -> Eff (EFQ.Query t qFrontend ': EFRQ.Requester t req Identity ': es) b
     -- ^ Child widget effect stack — carries the two capabilities named
     -- by 'Rhyolite.Frontend.Effect.Widget.RhyoliteWidgetC'.
  -> Eff es (Dynamic t (AppWebSocket t qWire), b)
runRhyoliteWidgetE toWire url child = mdo
  -- Server-branch WebSocket placeholder: nothing on the wire, never
  -- connected. Discarded on the client branch; only kept so the
  -- returned 'Dynamic' has a first sample on server render. The type
  -- annotation pins @q ~ qWire@ so 'defAppWebSocket''s ambiguous type
  -- variable resolves against the caller's wire currency.
  let defWs :: AppWebSocket t qWire
      defWs = defAppWebSocket
  dAppWebSocket <- EPr.prerender @t @space @es (pure defWs)
                     (openWebSocketE @req url request'' nubbedVs)

  -- Peel the socket 'Dynamic' into its two component event streams.
  let (notification, response) = splitNotifResponse dAppWebSocket

  -- Match outgoing requests against incoming responses. 'request' comes
  -- from 'runRequesterEff' below — the recursion is what 'mdo' closes.
  -- @tagId@ is the wire tag; @rawRsp@ is the opaque Aeson payload.
  -- (Pattern variables renamed from MTL's shorter @t@ / @v@ to avoid
  -- shadowing the scoped type variable 't' of the compositor.)
  (request', response') <-
    EFRhyReq.matchResponsesWithRequestsE
      reqEncoder
      request
      (ffor response $ \(TaggedResponse tagId rawRsp) -> (tagId, rawRsp))

  -- Re-tag decoded outbound requests as 'TaggedRequest' for the wire.
  let request'' = fmap decodeRequestBatch request'

  -- Peel the child's Requester layer and Query layer. Requester is the
  -- outer peel ('response'' feeds its response channel); Query is the
  -- inner peel ('view' feeds its ambient query-result channel).
  (request, (b, vsInc)) <- EFRhyReq.runRequesterEff
    (fmapMaybe (traverseRequesterData (fmap Identity)) response')
    (runQueryTE @t @qFrontend child view)

  -- View-selector 'Dynamic' → wire currency → nubbed to avoid duplicate
  -- sends on identical successive snapshots.
  let vsDyn = incrementalToDynamic vsInc
  nubbedVs <- EFDyn.holdUniqDyn (mapQuery toWire <$> vsDyn)

  -- Fold the notification stream into the ambient view-selector-result
  -- 'Dynamic' the child sees. 'prerender' returns 'Dynamic t (Dynamic t x)',
  -- flatten with 'join' via the 'Monad (Dynamic t)' instance.
  view <- buildViewDyn @t @space @qFrontend @qWire @es toWire vsDyn notification

  pure (dAppWebSocket, b)
  where
    -- Per-request wire-encoder\/decoder pair fed to
    -- 'matchResponsesWithRequestsE'. Direct port of the MTL @reqEncoder@
    -- local: 'whichever' \@ToJSON witnesses @ToJSON (req a)@ from the
    -- @ForallF ToJSON req@ side of 'Rhyolite.Api.Request'; 'has' \@FromJSON
    -- witnesses @FromJSON a@ from the @Has FromJSON req@ side. Defined
    -- inline (not as a separate top-level) because the rank-2 @forall a@
    -- interacts with the outer 'Request req' constraint in a way that's
    -- most-natural to state at the binding site — same shape MTL used.
    reqEncoder :: forall a. req a -> (Value, Value -> Maybe a)
    reqEncoder r =
      ( whichever @ToJSON @req @a (Aeson.toJSON r)
      , \x -> case has @FromJSON r (Aeson.fromJSON x) of
          Success s -> Just s
          _         -> Nothing
      )


-- | Server-branch placeholder 'AppWebSocket' for the 'prerender' call
-- in 'runRhyoliteWidgetE'. Same body as the MTL 'defAppWebSocket' local:
-- no events fire, connection reports 'False'.
defAppWebSocket :: forall t q. Reflex t => AppWebSocket t q
defAppWebSocket = AppWebSocket
  { _appWebSocket_notification = never
  , _appWebSocket_response     = never
  , _appWebSocket_version      = never
  , _appWebSocket_connected    = R.constDyn False
  }


-- | Fan the 'Dynamic t (AppWebSocket t q)' into its notification and
-- response event streams. Same body as the MTL local:
-- @bimap (switch . current) (switch . current) . splitDynPure $ ffor ...@.
--
-- The 'switch . current' pair is idiomatic reflex for "flatten a
-- 'Dynamic t (Event t a)' into an 'Event t a' by watching the current
-- inner event". Ported unchanged.
splitNotifResponse
  :: forall t q. Reflex t
  => Dynamic t (AppWebSocket t q)
  -> (Event t (QueryResult q), Event t TaggedResponse)
splitNotifResponse dAws =
  bimap (switch . current) (switch . current) $
    splitDynPure $
      ffor dAws $ \aws ->
        ( _appWebSocket_notification aws
        , _appWebSocket_response     aws
        )


-- | Re-tag a batch of decoded outbound wire requests
-- (@Map Int Aeson.Value@ from 'matchResponsesWithRequestsE') as a
-- list of 'TaggedRequest's ready to hand to 'openWebSocketE'. Direct
-- port of the MTL @request''@ local — drops entries whose @Aeson.Value@
-- fails to decode as @'Some' req@.
decodeRequestBatch
  :: forall req. Request req
  => Map.Map Int Value
  -> [TaggedRequest req]
decodeRequestBatch batch =
  Map.elems $
    Map.mapMaybeWithKey (\tag_ v ->
      case Aeson.fromJSON v of
        Success (someReq :: Some req) -> Just (TaggedRequest tag_ someReq)
        _                             -> Nothing) batch


-- | Fold the notification stream into the ambient view-selector-result
-- 'Dynamic' the child widget consumes. Wraps the 'fromNotificationsE'
-- call in a 'prerender' (client-only — server branch is 'pure mempty'
-- since notifications don't fire on the server) then flattens the
-- resulting @Dynamic t (Dynamic t (QueryResult qFrontend))@ with 'join'
-- (via the 'Monad (Dynamic t)' instance from the effectful reflex core).
buildViewDyn
  :: forall t space qFrontend qWire es.
     ( Reflex t
     , EFH.Hold t :> es
     , EFPE.PerformEventH t :> es
     , EFTE.TriggerEvent t :> es
     , DomBuilder t space :> es
     , EPr.HasPrerender t space es
     , EFPE.PerformEventH t :> EPr.ClientEs t space es
     , EFPE.PerformEventM (RH.HostFrame t) t :> EPr.ClientEs t space es
     , EFPE.PerformableForRow (EPr.ClientEs t space es) ~ RH.HostFrame t
     , EFTE.TriggerEvent t :> EPr.ClientEs t space es
     , EFH.Hold t :> EPr.ClientEs t space es
     , EFS.Sample t :> EPr.ClientEs t space es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , RQC.Query qFrontend
     , Monoid (QueryResult qFrontend)
     )
  => QueryMorphism qFrontend qWire
  -> Dynamic t qFrontend
  -> Event t (QueryResult qWire)
  -> Eff es (Dynamic t (QueryResult qFrontend))
buildViewDyn toWire vsDyn notification =
  fmap join $
    EPr.prerender @t @space @es (pure mempty) $
      fromNotificationsE vsDyn (mapQueryResult toWire <$> notification)


-- -----------------------------------------------------------------------------
-- 'runJengaRhyoliteWidgetE'
--
-- Effectful sibling of the deleted MTL
-- 'Rhyolite.Frontend.App.runJengaRhyoliteWidget'.
--
-- MTL body:
--
-- @
-- runJengaRhyoliteWidget toWire configRoute enc listenRoute child = do
--   obR <- askRoute
--   r'  <- fmap (parseURI . T.unpack . T.strip . T.decodeUtf8)
--             \<$\> getConfig configRoute
--   let route = case r' of
--         Nothing        -> error \"route config missing: ...\"
--         Just Nothing   -> error \"malformed confing route: ...\"
--         Just (Just r)  -> r
--   let wsUrl = T.pack (show (websocketUri route))
--             \<\> renderObeliskBackendRoute enc listenRoute
--   lift $ runRhyoliteWidget toWire wsUrl $ flip runRoutedT obR child
-- @
--
-- The E-side port drops the @askRoute + runRoutedT@ dance — the caller's
-- effect row already carries whichever 'Routed' effect it needs; there
-- is no transformer to lift through. The two 'error' sites become
-- @Left 'ObeliskRouteConfigError'@; the happy path delegates to
-- 'runRhyoliteWidgetE' with a constructed @wsUrl@.
-- -----------------------------------------------------------------------------

-- | Effectful port of the deleted MTL
-- 'Rhyolite.Frontend.App.runJengaRhyoliteWidget'.
--
-- Reads the backend base URL from the config map under @configRoute@
-- (via 'Jenga.Frontend.Effectful.Configs.getConfig'), parses it as a
-- 'URI' via 'Network.URI.parseURI', then constructs the WebSocket URL
-- by appending the rendered "listen" backend route.
--
-- Route-config lookup or parse failures surface as
-- @Left 'ObeliskRouteConfigError'@ — the compositor never crashes on
-- these; the caller decides how to render / retry / fall back.
runJengaRhyoliteWidgetE
  :: forall qFrontend qWire req es t space frontendRoute backendRoute b.
     ( RunRhyoliteWidgetEC qFrontend qWire req t space es
     , JCfg.Configs :> es
     )
  => QueryMorphism qFrontend qWire
     -- ^ Wire-format morphism. Same shape as 'runRhyoliteWidgetE'.
  -> Text
     -- ^ Config-file key naming the backend URL text file
     -- (conventionally @\"common/route\"@).
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName
     -- ^ Checked route encoder; used to render the "listen" backend
     -- route to a URL path.
  -> R backendRoute
     -- ^ Concrete backend "listen" route
     -- (@Rhyolite.Backend.App.serveDbOverWebsockets@ handles this on
     -- the server side).
  -> Eff (EFQ.Query t qFrontend ': EFRQ.Requester t req Identity ': es) b
     -- ^ Child widget stack.
  -> Eff es (Either ObeliskRouteConfigError (Dynamic t (AppWebSocket t qWire), b))
runJengaRhyoliteWidgetE toWire configRoute enc listenRoute child = do
  mbytes <- JCfg.getConfig configRoute
  case resolveWsBaseUri configRoute mbytes of
    Left err -> pure (Left err)
    Right baseUri ->
      let wsUrl = T.pack (show (websocketUri baseUri))
               <> renderObeliskBackendRoute enc listenRoute
      in Right <$> runRhyoliteWidgetE @qFrontend @qWire @req @es @t @space toWire wsUrl child


-- | Interpret a config-lookup result as either a resolved base 'URI' or
-- a specific 'ObeliskRouteConfigError'. Handles three failure sources:
--
--   * Missing key         → 'ObeliskRouteConfigMissing'.
--   * Non-UTF-8 payload   → 'ObeliskRouteConfigMalformed'.
--   * UTF-8 but not a URI → 'ObeliskRouteConfigMalformed'.
--
-- The middle case is a strict improvement over the MTL body, which used
-- 'Data.Text.Encoding.decodeUtf8' (that throws on invalid UTF-8) and
-- would produce a runtime crash. The E-side uses 'TE.decodeUtf8'' to
-- bubble the decode error out of the pure interpreter — same
-- "bubble up errors" principle the module docstring references.
--
-- Split out as a pure helper so the effectful compositor stays a
-- single-branch @case@ over the resolved value.
resolveWsBaseUri
  :: Text
  -> Maybe ByteString
  -> Either ObeliskRouteConfigError URI
resolveWsBaseUri configRoute = \case
  Nothing    -> Left (ObeliskRouteConfigMissing configRoute)
  Just bytes ->
    case TE.decodeUtf8' bytes of
      Left  _   -> Left (ObeliskRouteConfigMalformed configRoute)
      Right txt ->
        case parseURI (T.unpack (T.strip txt)) of
          Nothing  -> Left (ObeliskRouteConfigMalformed configRoute)
          Just uri -> Right uri
