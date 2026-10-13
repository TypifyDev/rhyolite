{-| Description: Effectful-side runners for Rhyolite frontend.

Effectful ports of the old MTL runners: 'fromNotificationsE', 'openWebSocketE'.
Kept in a separate module so its 'Reflex.Class' / 'Reflex.Host.HeadlessEffectful'
imports do not clash with the MTL surface in "Rhyolite.Frontend.App".
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rhyolite.Frontend.App.Effectful
  ( fromNotificationsE
  , openWebSocketE
  ) where

import qualified Data.Aeson             as Aeson
import           Data.Aeson             (FromJSON, ToJSON, decodeStrict')
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Effectful                                        as EFF
import           Effectful                                        (Eff, (:>))
import           Reflex.Class
                    ( Reflex, Event, Dynamic
                    , leftmost, fmapMaybe, fforMaybe, ffor
                    , attach, tag, current, updated, never
                    )
import qualified Reflex.Host.HeadlessEffectful    as EFH
import qualified Reflex.Host.HeadlessEffectful    as EFPB
import qualified Reflex.Host.HeadlessEffectful    as EFTE
import qualified Reflex.Host.HeadlessEffectful    as EFPE
import qualified Reflex.Effectful.DomHost         as EFJSM
import qualified Reflex.Host.Class                as RH
import qualified Reflex.Time                      as EFTime
import           Control.Monad.IO.Class           (MonadIO)
import           Reflex.Query.Class
                    (Query, QueryResult, crop)
import           Reflex.Effectful.DomHost
                    ( WebSocketConfig (..)
                    , RawWebSocket (..)
                    , webSocket
                    )
import           Rhyolite.Api           (Request)
import           Rhyolite.WebSocket
                    ( TaggedRequest
                    , WebSocketRequest (..), WebSocketResponse (..) )
import           Rhyolite.Frontend.App  (AppWebSocket (..))
fromNotificationsE
  :: forall es t q.
     (
       Query q
     , EFH.Hold t :> es
     , EFH.Sample t :> es
     , EFPE.PerformEventH t :> es
     , EFPE.PerformEventM (RH.HostFrame t) t :> es
     , EFPE.PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , EFTE.TriggerEvent t :> es
     , Reflex t
     , Monoid (QueryResult q)
     )
  => Dynamic t q
  -> Event t (QueryResult q)
  -> Eff es (Dynamic t (QueryResult q))
fromNotificationsE vs ePatch = do
  ePatchThrottled <- EFTime.throttleBatchWithLag lag ePatch
  EFH.foldDyn (\(vs', p) v -> crop vs' $ p <> v) mempty $
    attach (current vs) ePatchThrottled
  where
    lag :: Event t () -> Eff es (Event t ())
    lag e = EFPE.performEventH $ ffor e $ \_ -> pure ()

-- | Effectful port of the old MTL 'openWebSocket'. Delegates the socket
-- lifecycle to the Effectful 'webSocket' from
-- "Reflex.Dom.Core" and threads outbound requests /
-- view-selector updates through 'WebSocketConfig'. Decodes each incoming
-- payload and demuxes into the 'AppWebSocket' fields (notification /
-- response / version / connected).
openWebSocketE
  :: forall r q t es.
     (
       Reflex t
     , t ~ EFJSM.DomBuilderTimelineE es
     , EFJSM.JSM' :> es
     , EFPB.PostBuild t :> es
     , EFTE.TriggerEvent t :> es
     , EFPE.PerformEventH t :> es
     , EFPE.PerformEventM (RH.HostFrame t) t :> es
     , EFPE.PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , EFH.Hold t :> es
     , EFH.Sample t :> es
     , EFF.IOE :> es
     , FromJSON (QueryResult q)
     , ToJSON q
     , Request r
     )
  => Text
  -> Event t [TaggedRequest r]
  -> Dynamic t q
  -> Eff es (AppWebSocket t q)
openWebSocketE url request vs = mdo
  -- The Effectful 'webSocket' always yields a 'RawWebSocket t ByteString'
  -- (via 'onBSMessage'), so incoming payloads are decoded uniformly.
  let platformDecode = decodeStrict'
  let wsConfig :: WebSocketConfig t Text
      wsConfig = WebSocketConfig
        { _webSocketConfig_send =
            fmap (map (decodeUtf8 . LBS.toStrict . Aeson.encode)) $ mconcat
              [ fmap (map WebSocketRequest_Api) request
              , fmap ((:[]) . WebSocketRequest_ViewSelector) (updated vs)
                  :: Event t [WebSocketRequest q r]
              , tag (fmap ((:[]) . WebSocketRequest_ViewSelector) $ current vs)
                    (_webSocket_open ws)
              ]
        , _webSocketConfig_close = never
        , _webSocketConfig_reconnect = True
        , _webSocketConfig_protocols = []
        }
  ws <- webSocket url wsConfig
  let eMessages :: Event t (WebSocketResponse q)
      eMessages = fmapMaybe platformDecode (_webSocket_recv ws)
      notification = fforMaybe eMessages $ \case
        WebSocketResponse_View v -> Just v
        _                        -> Nothing
      response = fforMaybe eMessages $ \case
        WebSocketResponse_Api r -> Just r
        _                       -> Nothing
      version = fforMaybe eMessages $ \case
        WebSocketResponse_Version v -> Just v
        _                           -> Nothing
  connected <- EFH.holdDyn False $ leftmost
    [ True  <$ _webSocket_open ws
    , False <$ _webSocket_close ws
    ]
  return AppWebSocket
    { _appWebSocket_notification = notification
    , _appWebSocket_response     = response
    , _appWebSocket_version      = version
    , _appWebSocket_connected    = connected
    }
