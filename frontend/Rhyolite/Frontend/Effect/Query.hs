{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Description: Interpreter for 'Reflex.Effectful.Effect.Query.Query' via a
supplied 'QueryBridge' record.

= What this module provides

The effect definition and MTL-style helpers live in
"Reflex.Effectful.Effect.Query"; this module supplies the interpreter that
delegates each effect operation to a caller-supplied 'QueryBridge'. The
bridge is three call-backs: one for each 'MonadQuery' method.

= What this module deliberately does NOT provide

Bridging an 'Eff' program to a live 'Reflex.Query.Class.MonadQuery' instance
(e.g. 'Reflex.Query.Base.QueryT' or 'Rhyolite.Frontend.App.RhyoliteWidget')
cannot honestly be done by extracting per-operation @IO@ callbacks: the MTL
monad is 'Control.Monad.Writer'-shaped and accumulates collector state
across operations, and the state does not survive being called out of an
ephemeral @IO@ context.

The real production bridge belongs in the widget runner
('Rhyolite.Frontend.App.runRhyoliteWidget') and requires helpers that
another agent owns (see the port-report block in
"Rhyolite.Frontend.App"). Rather than ship a 'MonadQuery'-shim that pretends
to work but silently drops the collector, we ship only what CAN be honest:
the 'QueryBridge' record and the corresponding 'runQuery' interpreter, so
consumers of the effect can supply their own bridge (real, from the runner
once it exists; or hand-rolled for testing / mocking).
-}
module Rhyolite.Frontend.Effect.Query
  ( -- * The bridge
    QueryBridge(..)
    -- * Interpreter
  , runQuery
    -- * Watch combinators (Eff port of Rhyolite.Frontend.App.watch)
  , FullPath
  , queryDyn
  , watchViewSelector
  , watchEff
    -- * Vessel path DSL re-exports
  , module Data.Vessel.Path
    -- * Re-exports (Query effect + smart constructors)
  , Query (..), askQueryResultE, tellQueryIncrementalE
  ) where

import           Control.Monad.Fix            (MonadFix)
import           Data.Functor.Const           (Const (..))
import           Data.Functor.Identity        (Identity, runIdentity)
import           Data.Semigroup.Commutative   (Commutative)
import           Data.Patch                   (AdditivePatch (..), Group (..))
import           Effectful                    (Eff, IOE, (:>), liftIO)
import           Effectful.Dispatch.Dynamic   (interpret_)

import           Data.Vessel.Path

import           Reflex.Class
                    ( Reflex, Dynamic, Incremental, current, updated
                    , attachWith, sample, unsafeBuildIncremental
                    , zipDynWith
                    )
import qualified Reflex.Query.Class as Q
import           Reflex.Query.Class (QueryResult, SelectedCount, crop)
import           Reflex.Dynamic (holdUniqDyn)
import           Reflex.Host.HeadlessEffectful
                    ( Hold, Sample, Query (..)
                    , askQueryResultE, tellQueryIncrementalE
                    )

-- | A record of the three 'MonadQuery' operations, packaged so an
-- interpreter can call them without needing an ambient MTL constraint on
-- 'Eff' (which is impossible — 'Eff' is not a 'MonadQuery').
--
-- Each field is the exact 'MonadQuery' method already "run enough" to
-- produce a plain reflex value we can hand back through the effect.
--
-- IMPORTANT: constructing a legitimate 'QueryBridge' requires access to the
-- surrounding 'Reflex.Query.Base.QueryT' collector state, so in practice
-- this record is built by the widget runner (blocked on other agents; see
-- the port-report block in "Rhyolite.Frontend.App"). For testing you can
-- hand-roll a bridge whose bodies return 'never'-derived stubs.
data QueryBridge t q = QueryBridge
  { _queryBridge_tellQueryIncremental
      :: Incremental t (AdditivePatch q) -> IO ()
  , _queryBridge_askQueryResult
      :: IO (Dynamic t (QueryResult q))
  , _queryBridge_queryIncremental
      :: Incremental t (AdditivePatch q)
      -> IO (Dynamic t (QueryResult q))
  }

-- | Interpret 'Query' by delegating each operation to the corresponding
-- field of the supplied 'QueryBridge'. The bridge is treated as fixed for
-- the duration of the interpretation.
runQuery
  :: forall t q es a.
     ( IOE :> es )
  => QueryBridge t q
  -> Eff (Query t q ': es) a
  -> Eff es a
runQuery bridge = interpret_ $ \case
  TellQueryIncremental inc -> liftIO $ _queryBridge_tellQueryIncremental bridge inc
  AskQueryResult           -> liftIO $ _queryBridge_askQueryResult bridge
  QueryIncremental inc     -> liftIO $ _queryBridge_queryIncremental bridge inc

--------------------------------------------------------------------------------
-- Watch combinators — Eff port of Rhyolite.Frontend.App.watch
--
-- Mirrors the MTL trio (@tellQueryDyn@ / @queryDyn@ / @watchViewSelector@ /
-- @watch@) verbatim in shape. The MTL @MonadQuery t q m@ constraint becomes
-- @Query t q :> es@; @MonadHold t m@ becomes @Hold t :> es@; @MonadFix m@
-- is satisfied by @Eff@'s built-in @MonadFix@ instance.
--
-- The Eff 'Query' effect's primitive payloads are Core-native
-- (@PrimInc.Incremental Core.Global ...@), so we pin @t ~ EffTimeline Global@
-- via a @KnownTimeline es t@ constraint on the effect stack and wrap the
-- data-family @Incremental t p@ / @Dynamic t a@ through
-- @EffIncremental@ / @EffDynamic@ at the effect boundary. This is the same
-- bridging strategy the sibling 'Reflex.Effectful.Collection.holdUniqDynEff'
-- uses.
--------------------------------------------------------------------------------

-- | The type of 'Path' required to do a 'watch' — trivial re-export of the
-- MTL synonym, unchanged.
type FullPath a v b =
  Path (Const SelectedCount a) (v (Const SelectedCount)) (v Identity) (Identity b)

-- | Eff-shaped 'tellQueryDyn'. Verbatim port of
-- 'Reflex.Effectful.Class.Query.Class.tellQueryDyn' (which itself is
-- Rhyolite's MTL @tellQueryDyn@).
tellQueryDynE
  :: forall t q es
   . ( Reflex t
     , Query t q :> es
     , Group q
     , Commutative q
     , Q.Query q
     )
  => Dynamic t q
  -> Eff es ()
tellQueryDynE d = do
  let inc = unsafeBuildIncremental
        (sample (current d))
        (attachWith (\old new -> AdditivePatch $ new ~~ old) (current d) (updated d))
  tellQueryIncrementalE @t @q inc

-- | Eff-shaped 'queryDyn'. Verbatim port of MTL @queryDyn@:
--
-- > queryDyn q = do
-- >   tellQueryDyn q
-- >   zipDynWith crop q <$> askQueryResult
--
-- Wraps the Core-native @Dynamic Core.Global (QueryResult q)@ that
-- 'askQueryResult' hands back into the data-family @Dynamic t@ wrapper via
-- @EffDynamic@ so the returned dynamic composes with the rest of the Eff
-- reflex surface.
queryDyn
  :: forall t q es
   . ( Reflex t
     , Query t q :> es
     , Group q
     , Commutative q
     , Q.Query q
     )
  => Dynamic t q
  -> Eff es (Dynamic t (QueryResult q))
queryDyn q = do
  tellQueryDynE @t @q q
  r <- askQueryResultE @t @q
  pure $ zipDynWith crop q r

-- | Eff-shaped 'watchViewSelector'. Same as MTL: dedup a 'queryDyn' via
-- 'holdUniqDynEff'.
watchViewSelector
  :: forall t q es
   . ( Reflex t
     , Query t q :> es
     , Hold t :> es
     , Sample t :> es
     , Group q
     , Commutative q
     , Q.Query q
     , Eq (QueryResult q)
     , MonadFix (Eff es)
     )
  => Dynamic t q
  -> Eff es (Dynamic t (QueryResult q))
watchViewSelector = holdUniqDyn <=. queryDyn
  where
    -- Local Kleisli-style compose so the point-free reads exactly like MTL:
    --   watchViewSelector = holdUniqDyn <=< queryDyn
    (<=.) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
    f <=. g = \x -> g x >>= f

-- | Eff-shaped 'watch'. Verbatim shape port of
-- 'Rhyolite.Frontend.App.watch' — same @Path@-driven query construction and
-- result unwrapping, transliterated from MTL @m@ to @Eff es@. Named
-- 'watchEff' to keep it obviously distinct from the MTL 'watch'.
watchEff
  :: forall t v a b es
   . ( Reflex t
     , Query t (v (Const SelectedCount)) :> es
     , Hold t :> es
     , Sample t :> es
     , Group  (v (Const SelectedCount))
     , Commutative (v (Const SelectedCount))
     , Q.Query (v (Const SelectedCount))
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Eq (v Identity)
     , MonadFix (Eff es)
     )
  => Dynamic t (FullPath a v b)
  -> Eff es (Dynamic t (Maybe b))
watchEff pathDyn = do
  r <- watchViewSelector @t @(v (Const SelectedCount)) $
         fmap (\path -> _path_to path (Const 1)) pathDyn
  pure $ zipDynWith (\path res -> fmap runIdentity (_path_from path res)) pathDyn r

-- (Removed: coreDynToDataFamily. The old data-family newtype wrapping
-- Core.Global into an EffTimeline wrapper is gone — Reflex.Class's
-- Dynamic t / Incremental t are used bare across the port.)
