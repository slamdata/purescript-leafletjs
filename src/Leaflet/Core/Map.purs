module Leaflet.Core.Map
  ( leaflet
  , setView
  , setZoom
  , getSize
  , zoomAnimation
  , getPanes
  , containerPointToLayerPoint
  , getZoomScale
  , getMapPanePos
  , getCenterOffset
  , getMaxZoom
  , getZoom
  , latLngToContainerPoint
  , invalidateSize
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import DOM (DOM)
import DOM.Node.Types (Element)
import DOM.Classy.Element (class IsElement, toElement)

import Leaflet.Core.Converter (converter)
import Leaflet.Core.Types as T
import Leaflet.Util ((∘), (×))

foreign import map_
  ∷ ∀ e. Element → Eff (dom ∷ DOM|e) T.Leaflet

foreign import setView_
  ∷ ∀ e. Fn2 (Array T.Degrees) T.Leaflet (Eff (dom ∷ DOM|e) T.Leaflet)

foreign import setZoom_
  ∷ ∀ e. Fn2 T.Zoom T.Leaflet (Eff (dom ∷ DOM|e) T.Leaflet)

foreign import getSize_
  ∷ ∀ e. Fn2 (Int → Int → T.Point) T.Leaflet (Eff (dom ∷ DOM|e) T.Point)

foreign import zoomAnimation_
  ∷ ∀ e. T.Leaflet → Eff (dom ∷ DOM|e) Boolean

foreign import getPanes_
  ∷ ∀ e. T.Leaflet → Eff (dom ∷ DOM|e) (SM.StrMap Element)

foreign import containerPointToLayerPoint_
  ∷ ∀ e. Fn3 (Int → Int → T.Point) (Array Int) T.Leaflet (Eff (dom ∷ DOM|e) T.Point)

foreign import latLngToContainerPoint_
  ∷ ∀ e. Fn3 (Int → Int → T.Point) (Array T.Degrees) T.Leaflet (Eff (dom ∷ DOM|e) T.Point)

foreign import getZoomScale_
  ∷ ∀ e. Fn2 T.Zoom T.Leaflet (Eff (dom ∷ DOM|e) Number)

foreign import getMapPanePos_
  ∷ ∀ e. Fn2 (Int → Int → T.Point) T.Leaflet (Eff (dom ∷ DOM|e) T.Point)

foreign import getCenterOffset_
  ∷ ∀ e. Fn3 (Int → Int → T.Point) (Array Int) T.Leaflet (Eff (dom ∷ DOM|e) T.Point)

foreign import getMaxZoom_
  ∷ ∀ e. T.Leaflet → Eff (dom ∷ DOM|e) T.Zoom

foreign import getZoom_
  ∷ ∀ e. T.Leaflet → Eff (dom ∷ DOM|e) T.Zoom

foreign import invalidateSize_
  ∷ ∀ e. Fn2 Boolean T.Leaflet (Eff (dom ∷ DOM|e) T.Leaflet)


leaflet
  ∷ ∀ n e m
  . IsElement n
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ n
  → m T.Leaflet
leaflet = liftEff ∘ map_ ∘ toElement

setView
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.LatLng
  → T.Leaflet
  → m T.Leaflet
setView latLng l =
  liftEff $ runFn2 setView_ (converter.convertLatLng latLng) l

setZoom
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Zoom
  → T.Leaflet
  → m T.Leaflet
setZoom i l =
  liftEff $ runFn2 setZoom_ i l

getSize
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → m T.Point
getSize l =
  liftEff $ runFn2 getSize_ Tuple l

zoomAnimation
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → m Boolean
zoomAnimation = liftEff ∘ zoomAnimation_

getPanes
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → m (SM.StrMap Element)
getPanes = liftEff ∘ getPanes_


containerPointToLayerPoint
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Point
  → T.Leaflet
  → m T.Point
containerPointToLayerPoint (a × b) l =
  liftEff $ runFn3 containerPointToLayerPoint_ Tuple [a, b] l

latLngToContainerPoint
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.LatLng
  → T.Leaflet
  → m T.Point
latLngToContainerPoint {lat, lng} l =
  liftEff $ runFn3 latLngToContainerPoint_ Tuple [lat, lng] l

getZoomScale
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Zoom
  → T.Leaflet
  → m Number
getZoomScale zoom l =
  liftEff $ runFn2 getZoomScale_ zoom l

getMapPanePos
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → m T.Point
getMapPanePos l =
  liftEff $ runFn2 getMapPanePos_ Tuple l

getCenterOffset
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Point
  → T.Leaflet
  → m T.Point
getCenterOffset (a × b) l =
  liftEff $ runFn3 getCenterOffset_ Tuple [a, b] l

getMaxZoom
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → m T.Zoom
getMaxZoom = liftEff ∘ getMaxZoom_

getZoom
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → m T.Zoom
getZoom = liftEff ∘ getZoom_

invalidateSize
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Boolean
  → T.Leaflet
  → m T.Leaflet
invalidateSize b l =
  liftEff $ runFn2 invalidateSize_ b l
