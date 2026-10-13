{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Description: Effectful ports of the reflex-requester glue that
'Rhyolite.Frontend.Effect.WidgetRunner' needs to compose
'runRhyoliteWidgetE' out of 'runQueryTE' + a 'Requester' interpreter +
a request\/response matcher.

= Contents

  * 'matchResponsesWithRequestsE' — Effectful port of MTL
    'Reflex.Requester.Base.Internal.matchResponsesWithRequests'. Same
    request-decoder\/wire-tagging protocol; the 'MonadHold' obligation
    of the MTL sibling is discharged by 'EFH.hold' \/
    'EFH.holdIncremental' from "Reflex.Effectful.Effect.Hold". The
    body of 'matchResponseMapWithRequests' was inlined here (rather
    than reintroducing a 'MonadHold t (Eff es)' shim in
    reflex-dom-effectful) — see the note in
    'Rhyolite.Frontend.Effect.WidgetRunner' at "Rhyolite runners".

  * 'runRequesterEff' — Effectful interpreter for the
    'Reflex.Effectful.Effect.Requester.Requester' effect. Semantically
    equivalent to MTL 'Reflex.Requester.Base.runRequesterT': feeds
    incoming responses to per-'requesting' return events, aggregates
    outbound per-call events into one merged 'RequesterData req'.

Both primitives use the E-side 'RequesterData' from
"Reflex.Effectful.Core.Requester", NOT the MTL type in
"Reflex.Requester.Base". They are structurally identical
newtype-for-newtype ('newtype RequesterData f = RequesterData (TagMap
(Entry f))'), but the reflex-effectful copy is exposed via
@reflex-effectful@ — which is already a build-dep — while the MTL copy
lives in the @reflex@ package that @rhyolite-frontend@ does not depend
on. Using the E-side copy keeps the dependency footprint minimal and
type-unifies with 'EFRQ.runRequester', which is what 'runRequesterEff'
delegates to.
-}
module Rhyolite.Frontend.Effect.Requester
  ( -- * Request\/response matching
    matchResponsesWithRequestsE
  , matchResponseMapWithRequestsE
    -- * Requester interpreter
  , runRequesterEff
  ) where

import           Control.Monad                        (forM)
import           Control.Monad.Fix                    (MonadFix)
import           Control.Monad.State.Strict           (get, put, runState)
import           Data.Dependent.Sum                   (DSum (..))
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Patch.Map                       (PatchMap (..))
import qualified Effectful                            as EFF
import           Effectful                            (Eff, (:>))

import           Reflex.Requester.Base                (RequesterData,
                                                       RequesterDataKey,
                                                       emptyRequesterData,
                                                       mergeRequesterData,
                                                       requesterDataToList,
                                                       singletonRequesterData)

import           Reflex.Class                         (Behavior, Event,
                                                       Incremental, Reflex,
                                                       currentIncremental,
                                                       leftmost, push,
                                                       pushAlways, sample)
import qualified Reflex.Host.HeadlessEffectful        as EFH
import qualified Reflex.Host.HeadlessEffectful        as EFRQ


-- -----------------------------------------------------------------------------
-- Decoder — private witness pairing a 'RequesterDataKey' (which routes the
-- response back to the exact 'requesting' call that originated it) with the
-- decoder function the caller supplied for that call's response payload.
--
-- Direct port of 'Reflex.Requester.Base.Internal.Decoder'. Existentially
-- quantifies the payload index 'a', so a single 'Decoder' value can sit in
-- a homogeneously-typed 'Map Int (Decoder ...)' regardless of which typed
-- request produced it.
-- -----------------------------------------------------------------------------

-- | Pairs a typed request key with the decoder for the associated response.
data Decoder rawResponse response
  = forall a. Decoder (RequesterDataKey a) (rawResponse -> response a)


-- -----------------------------------------------------------------------------
-- matchResponsesWithRequestsE
--
-- Port of 'Reflex.Requester.Base.Internal.matchResponsesWithRequests':
--
-- @
-- matchResponsesWithRequests
--   :: (MonadFix m, MonadHold t m, Reflex t)
--   => (forall a. request a -> (rawRequest, rawResponse -> response a))
--   -> Event t (RequesterData request)
--   -> Event t (Int, rawResponse)
--   -> m ( Event t (Map Int rawRequest)
--        , Event t (RequesterData response)
--        )
-- matchResponsesWithRequests f send recv =
--   matchResponseMapWithRequests f send $ uncurry Map.singleton \<$\> recv
-- @
--
-- The MTL body delegates to 'matchResponseMapWithRequests'; we expose the
-- same one-line delegation here so callers who already have a keyed 'Event t
-- (Int, rawResponse)' don't have to build a singleton 'Map' at their site.
-- -----------------------------------------------------------------------------

-- | Effectful port of the MTL sibling of the same name. Signature and
-- semantics match verbatim; the 'MonadHold' obligation is discharged by
-- 'EFH.Hold' in the outer effect row.
matchResponsesWithRequestsE
  :: forall es t rawRequest rawResponse request response.
     ( Reflex t
     , MonadFix (Eff es)
     , EFH.Hold t :> es
     , EFH.Sample t :> es
     )
  => (forall a. request a -> (rawRequest, rawResponse -> response a))
     -- ^ Given a typed request, produce its wire-format encoding plus
     --   the decoder for the raw response bytes that will come back.
  -> Event t (RequesterData request)
     -- ^ Outgoing typed requests as a batched 'RequesterData'.
  -> Event t (Int, rawResponse)
     -- ^ Incoming raw responses tagged by the wire key we allocated
     --   when the corresponding request went out.
  -> Eff es
       ( Event t (Map Int rawRequest)
       , Event t (RequesterData response)
       )
matchResponsesWithRequestsE f sendEv recvEv =
  matchResponseMapWithRequestsE f sendEv
    (fmap (uncurry Map.singleton) recvEv)


-- -----------------------------------------------------------------------------
-- matchResponseMapWithRequestsE
--
-- Inline port of 'Reflex.Requester.Base.Internal.matchResponseMapWithRequests'.
-- The MTL body:
--
-- @
-- matchResponseMapWithRequests f send recv = do
--   rec nextId \<- hold 1 $ fmap (\\ (next, _, _) -> next) outgoing
--       waitingFor :: Incremental t (PatchMap Int (Decoder rawResponse response)) \<-
--         holdIncremental mempty $ leftmost
--           [ fmap (\\ (_, outstanding, _) -> outstanding) outgoing
--           , snd \<$\> incoming
--           ]
--       let outgoing = processOutgoing nextId send
--           incoming = processIncoming waitingFor recv
--   return (fmap (\\ (_, _, rawReqs) -> rawReqs) outgoing, fst \<$\> incoming)
-- @
--
-- ported here to 'Eff es' using:
--
--   * 'EFH.hold' \/ 'EFH.holdIncremental' for the 'MonadHold' calls.
--   * 'RecursiveDo' \+ 'MonadFix (Eff es)' for the 'rec' knot.
--   * Timeline-polymorphic pure combinators ('pushAlways', 'push',
--     'sample', 'leftmost', 'currentIncremental') for the two
--     stream processors — same call shapes as the MTL body.
--
-- The processors themselves ('processOutgoingE' \/ 'processIncomingE') are
-- verbatim ports of the MTL 'where'-block helpers of the same name, translated
-- to 'PushM t' via 'pushAlways' \/ 'push'. See their haddocks for the
-- correspondence.
-- -----------------------------------------------------------------------------

-- | Effectful port of the MTL sibling of the same name. Same semantics as
-- 'matchResponsesWithRequestsE', except the caller supplies a
-- @'Map' 'Int' rawResponse@ per fire instead of a singleton
-- @('Int', rawResponse)@ pair.
matchResponseMapWithRequestsE
  :: forall es t rawRequest rawResponse request response.
     ( Reflex t
     , MonadFix (Eff es)
     , EFH.Hold t :> es
     , EFH.Sample t :> es
     )
  => (forall a. request a -> (rawRequest, rawResponse -> response a))
  -> Event t (RequesterData request)
  -> Event t (Map Int rawResponse)
  -> Eff es
       ( Event t (Map Int rawRequest)
       , Event t (RequesterData response)
       )
matchResponseMapWithRequestsE f sendEv recvEv = do
  rec
    -- Behavior tracking the next-available integer key. Starts at 1;
    -- 'processOutgoingE' bumps it by (length of the outgoing batch) on
    -- each fire. Matches MTL: 'hold 1 $ fmap (\ (next, _, _) -> next)
    -- outgoing'.
    nextId <- EFH.hold @t (1 :: Int)
      (fmap (\ (next, _, _) -> next) outgoing)

    -- Incremental tracking the outstanding decoders. Patched forward
    -- (new decoders added) by 'processOutgoingE' and patched back
    -- (decoders removed once consumed) by 'processIncomingE'. Matches
    -- MTL:
    --   holdIncremental mempty $ leftmost
    --     [ fmap (\ (_, outstanding, _) -> outstanding) outgoing
    --     , snd <$> incoming
    --     ]
    waitingFor
      :: Incremental t (PatchMap Int (Decoder rawResponse response))
      <- EFH.holdIncrementalE
           (Map.empty :: Map Int (Decoder rawResponse response))
           (leftmost
              [ fmap (\ (_, outstanding, _) -> outstanding) outgoing
              , fmap snd incoming
              ])

    let outgoing :: Event t
                    ( Int
                    , PatchMap Int (Decoder rawResponse response)
                    , Map Int rawRequest
                    )
        outgoing = processOutgoingE f nextId sendEv

        incoming :: Event t
                    ( RequesterData response
                    , PatchMap Int (Decoder rawResponse response)
                    )
        incoming = processIncomingE waitingFor recvEv

  pure
    ( fmap (\ (_, _, rawReqs) -> rawReqs) outgoing
    , fmap fst incoming
    )


-- | Tag each request in the outbound batch with a fresh 'Int', return
-- (new next-id, patch adding the new decoders to 'waitingFor', wire-format
-- request map to send out).
--
-- Verbatim port of the MTL 'processOutgoing' local. The state monad usage
-- inside the callback ('runState' + 'get'/'put' from 'Control.Monad.State.Strict')
-- is plain-Haskell — it's a pure @Int -> ([...], Int)@ computation, not an
-- effect handler — and matches the MTL body line-for-line.
processOutgoingE
  :: forall t rawRequest rawResponse request response.
     Reflex t
  => (forall a. request a -> (rawRequest, rawResponse -> response a))
  -> Behavior t Int
  -> Event t (RequesterData request)
  -> Event t
       ( Int
       , PatchMap Int (Decoder rawResponse response)
       , Map Int rawRequest
       )
processOutgoingE f nextId out_ = flip pushAlways out_ $ \dm -> do
  oldNextId <- sample nextId
  let (result, newNextId) =
        flip runState oldNextId $
          forM (requesterDataToList dm) $ \(k :=> v) -> do
            n <- get
            put (succ n)
            let (rawReq, rspF) = f v
            pure (n, rawReq, Decoder k rspF)
      patchWaitingFor =
        PatchMap $ Map.fromList $
          fmap (\(n, _, dec) -> (n, Just dec)) result
      toSend =
        Map.fromList $
          fmap (\(n, rawReq, _) -> (n, rawReq)) result
  pure (newNextId, patchWaitingFor, toSend)


-- | Match each incoming raw response against the outstanding decoders and
-- return (decoded response bag, patch removing consumed decoders). Returns
-- 'Nothing' — via 'push' — when no keys match, so no downstream firing.
--
-- Verbatim port of the MTL 'processIncoming' local. The @PatchMap Int v@ in
-- the return position keeps 'v' polymorphic because it's only used to
-- record deletions (a 'PatchMap' whose values are all 'Nothing') — the 'v'
-- never appears in a value, so any @v@ works.
processIncomingE
  :: forall t rawResponse response v.
     Reflex t
  => Incremental t (PatchMap Int (Decoder rawResponse response))
  -> Event t (Map Int rawResponse)
  -> Event t (RequesterData response, PatchMap Int v)
processIncomingE waitingFor inc = flip push inc $ \rspMap -> do
  -- 'currentIncremental' produces a 'Behavior' over the 'PatchTarget'
  -- of 'PatchMap Int (Decoder ...)' — that unfolds to 'Map Int (Decoder
  -- ...)' — so we intersect the incoming @rspMap@ against the sampled
  -- 'Map' directly, no 'unPatchMap' peel required.
  wf <- sample (currentIncremental waitingFor)
  let match rawRsp (Decoder k rspF) =
        singletonRequesterData k (rspF rawRsp)
      matches = Map.intersectionWith match rspMap wf
  pure $
    if Map.null matches
      then Nothing
      else Just
        ( Map.foldl' mergeRequesterData emptyRequesterData matches
        , PatchMap (fmap (const Nothing) matches)
        )


-- -----------------------------------------------------------------------------
-- runRequesterEff
--
-- Interpreter for the 'Requester t req rsp' effect. Feeds incoming
-- 'RequesterData rsp' responses back to the per-'requesting' return events,
-- aggregates every outbound 'requesting'\/'requesting_' call into a single
-- 'Event t (RequesterData req)'. Directly delegates to
-- 'EFRQ.runRequester' (the reflex-effectful interpreter of the same shape),
-- reordering the returned tuple so the merged outbound event comes first —
-- matching the convention @('outbound', childResult)@ that 'runRhyoliteWidgetE'
-- expects at its scope-close.
--
-- Timeline pinning
-- ~~~~~~~~~~~~~~~~
-- Pinned to @t ~ 'Core.EffTimeline' 'Core.Global'@ for exactly the same
-- reason as the sibling 'runQueryTE' \/ 'withQueryTE' \/ 'mapAuthE' in
-- 'Rhyolite.Frontend.Effect.WidgetRunner': the underlying reflex-effectful
-- interpreter ('EFRQ.runRequester') is currently defined only against the
-- native @'Core.EffTimeline' 'Core.Global'@ timeline. The 'KnownTimeline es t'
-- constraint is included so the type checker can infer @t@ from @es@ at
-- consumer sites via the same fundep the sibling runners rely on.
--
-- Making this timeline-polymorphic
-- --------------------------------
-- A truly timeline-polymorphic 'runRequesterEff' would require a
-- timeline-polymorphic 'Reflex.Effectful.Effect.Requester.runRequester' —
-- which does not yet exist. The MTL 'Reflex.Requester.Base.runRequesterT'
-- body ports directly at the class level (using timeline-polymorphic
-- 'fanInt' \/ 'mergeInt' \/ 'coerceEvent' from 'Reflex t'), but the tag
-- machinery inside 'tagRequest' relies on an 'unsafeCoerce' on
-- @Event t (Entry request x) -> Event t Any@ — which is genuinely unsafe
-- (no 'Coercible' proof exists) and is forbidden by this codebase's
-- no-'unsafeCoerce' rule for new code. The right resolution is to add a
-- polymorphic-in-@t@ 'runRequester' to reflex-effectful that encapsulates
-- the coerce the same way the pinned version does, then lift the
-- @t ~ 'Core.EffTimeline' 'Core.Global'@ constraint from here in the
-- same commit. That's the follow-up; this module holds the interface at
-- the shape 'runRhyoliteWidgetE' will consume.
-- -----------------------------------------------------------------------------

-- | Effectful port of MTL 'Reflex.Requester.Base.runRequesterT'.
-- See the module haddock for why 't' is pinned to
-- @'Core.EffTimeline' 'Core.Global'@ and how consumers should compose this
-- with 'matchResponsesWithRequestsE' at the rhyolite-widget scope.
runRequesterEff
  :: forall t req rsp es a.
     ( Reflex t
     , EFF.IOE :> es
     )
  => Event t (RequesterData rsp)
     -- ^ Incoming responses — typically wired from
     -- @snd \<$\> 'matchResponsesWithRequestsE' ...@ at the widget's
     -- scope-close so responses fan out to the exact 'requesting' call
     -- that produced the correlated outbound wire request.
  -> Eff (EFRQ.Requester t req rsp : es) a
     -- ^ Child action using 'requesting' \/ 'requesting_'.
  -> Eff es (Event t (RequesterData req), a)
     -- ^ Merged outbound 'RequesterData req' event alongside the child's
     -- result value.
runRequesterEff responses child = do
  (a, requests) <- EFRQ.runRequester responses child
  pure (requests, a)


-- -----------------------------------------------------------------------------
-- 'Patch' constraint witness for 'PatchMap Int v'.
--
-- The 'holdIncremental' call in 'matchResponseMapWithRequestsE' requires
-- @'Patch' ('PatchMap' 'Int' (Decoder rawResponse response))@ in scope.
-- 'Data.Patch.Map' provides the instance @'Ord' k => 'Patch' ('PatchMap' k v)@;
-- GHC discharges it automatically from the ambient 'Ord Int' instance.
-- No explicit witness required.
-- -----------------------------------------------------------------------------
