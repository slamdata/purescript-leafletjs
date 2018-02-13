module Leaflet.Core.Layer
  ( layer
  , tileLayer
  , marker
  , icon
  , popup
  , setURI
  , setContent
  , setLatLng
  , openOn
  , bindPopup
  , openPopup
  , circleMarker
  , circle
  , polyline
  , polygon
  , rectangle
  , on
  , once
  , setIcon
  , addLayer
  , removeLayer
  , layerGroup
  , off
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Array as A
import Data.Foldable (class Foldable, foldMap)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.URI (URIRef)

import DOM (DOM)

import Leaflet.Core.Converter (ConvertDict, converter)
import Leaflet.Core.Types as T
import Leaflet.Util ((∘))

foreign import layer_
  ∷ ∀ e. Eff (dom ∷ DOM|e) T.Layer

foreign import tileLayer_
  ∷ ∀ e. String → Eff (dom ∷ DOM|e) T.TileLayer

foreign import marker_
  ∷ ∀ e. Array T.Degrees → Eff (dom ∷ DOM|e) T.Marker

foreign import icon_
  ∷ ∀ e r. Fn2 ConvertDict r (Eff (dom ∷ DOM|e) T.Icon)

foreign import popup_
  ∷ ∀ e r. Fn2 ConvertDict r (Eff (dom ∷ DOM|e) T.Popup)

foreign import setURI_
  ∷ ∀ e. Fn2 String T.TileLayer (Eff (dom ∷ DOM|e) T.TileLayer)

foreign import setLatLng_
  ∷ ∀ e. Fn2 (Array T.Degrees) T.Popup (Eff (dom ∷ DOM|e) T.Popup)

foreign import setContent_
  ∷ ∀ e. Fn2 String T.Popup (Eff (dom ∷ DOM|e) T.Popup)

foreign import openOn_
  ∷ ∀ e. Fn2 T.Leaflet T.Popup (Eff (dom ∷ DOM|e) T.Popup)

foreign import bindPopup_
  ∷ ∀ e. Fn2 String T.Layer (Eff (dom ∷ DOM|e) T.Layer)

foreign import openPopup_
  ∷ ∀ e. Fn3 Boolean (Array T.Degrees) T.Layer (Eff (dom ∷ DOM|e) T.Layer)

foreign import circleMarker_
  ∷ ∀ e r. Fn3 (Array T.Degrees) ConvertDict r (Eff (dom ∷ DOM|e) T.CircleMarker)

foreign import circle_
  ∷ ∀ e r. Fn3 (Array T.Degrees) ConvertDict r (Eff (dom ∷ DOM|e) T.Circle)

foreign import polyline_
  ∷ ∀ e r. Fn3 (Array (Array T.Degrees)) ConvertDict r (Eff (dom ∷ DOM|e) T.Polyline)

foreign import polygon_
  ∷ ∀ e r. Fn3 (Array (Array T.Degrees)) ConvertDict r (Eff (dom ∷ DOM|e) T.Polygon)

foreign import rectangle_
  ∷ ∀ e r. Fn3 (Array (Array T.Degrees)) ConvertDict r (Eff (dom ∷ DOM|e) T.Rectangle)

foreign import on_
  ∷ ∀ e. Fn3 String (T.Event → Eff (dom ∷ DOM|e) Unit) T.Evented (Eff (dom ∷ DOM|e) Unit)

foreign import off_
  ∷ ∀ e. Fn2 String T.Evented (Eff (dom ∷ DOM|e) Unit)

foreign import once_
  ∷ ∀ e. Fn3 String (T.Event → Eff (dom ∷ DOM|e) Unit) T.Evented (Eff (dom ∷ DOM|e) Unit)

foreign import setIcon_
  ∷ ∀ e. Fn2 T.Icon T.Marker (Eff (dom ∷ DOM|e) T.Marker)

foreign import addLayer_
  ∷ ∀ e. Fn2 T.Layer T.Leaflet (Eff (dom ∷ DOM|e) T.Leaflet)

foreign import removeLayer_
  ∷ ∀ e. Fn2 T.Layer T.Leaflet (Eff (dom ∷ DOM|e) T.Leaflet)

foreign import layerGroup_
  ∷ ∀ e. Array T.Layer → Eff (dom ∷ DOM|e) T.LayerGroup


layerGroup
  ∷ ∀ e m f
  . Foldable f
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ f T.Layer
  → m T.LayerGroup
layerGroup =
  liftEff ∘ layerGroup_ ∘ A.fromFoldable

layer
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ m T.Layer
layer = liftEff layer_

tileLayer
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ URIRef
  → m T.TileLayer
tileLayer =
  liftEff ∘ tileLayer_ ∘ converter.printURI

marker
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.LatLng
  → m T.Marker
marker latLng =
  liftEff $ marker_ $ converter.convertLatLng latLng

icon
  ∷ ∀ e r1 r2 m
  . Union r1 r2 T.IconConf
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ Record r1
  → m T.Icon
icon r =
  liftEff $ runFn2 icon_ converter r

popup
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 T.PopupConf
  ⇒ Record r1
  → m T.Popup
popup r =
  liftEff $ runFn2 popup_ converter r

setURI
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ URIRef
  → T.TileLayer
  → m T.TileLayer
setURI uri tl =
  liftEff $ runFn2 setURI_ (converter.printURI uri) tl

setLatLng
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.LatLng
  → T.Popup
  → m T.Popup
setLatLng latLng p =
  liftEff $ runFn2 setLatLng_ (converter.convertLatLng latLng) p

setContent
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → T.Popup
  → m T.Popup
setContent c p =
  liftEff $ runFn2 setContent_ c p

openOn
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → T.Popup
  → m T.Popup
openOn m p =
  liftEff $ runFn2 openOn_ m p

circleMarker
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 T.CircleConf
  ⇒ T.LatLng
  → Record r1
  → m T.CircleMarker
circleMarker ll r =
  liftEff $ runFn3 circleMarker_ (converter.convertLatLng ll) converter r

circle
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 T.CircleConf
  ⇒ T.LatLng
  → Record r1
  → m T.Circle
circle ll r =
  liftEff $ runFn3 circle_ (converter.convertLatLng ll) converter r

polyline
  ∷ ∀ e r1 r2 m f
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (T.PolylineConf ())
  ⇒ Foldable f
  ⇒ f T.LatLng
  → Record r1
  → m T.Polyline
polyline lls r =
  liftEff $ runFn3 polyline_ (foldMap (A.singleton ∘ converter.convertLatLng) lls) converter r

polygon
  ∷ ∀ e r1 r2 m f
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (T.PolylineConf ())
  ⇒ Foldable f
  ⇒ f T.LatLng
  → Record r1
  → m T.Polygon
polygon lls r =
  liftEff $ runFn3 polygon_ (foldMap (A.singleton ∘ converter.convertLatLng) lls) converter r

rectangle
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (T.PolylineConf ())
  ⇒ T.LatLng
  → T.LatLng
  → Record r1
  → m T.Rectangle
rectangle a b r =
  liftEff
  $ runFn3
      rectangle_
      [ converter.convertLatLng a, converter.convertLatLng b ]
      converter
      r
on
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → (T.Event → Eff (dom ∷ DOM|e) Unit)
  → T.Evented
  → m Unit
on e fn l =
  liftEff $ runFn3 on_ e fn l

once
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → (T.Event → Eff (dom ∷ DOM|e) Unit)
  → T.Evented
  → m Unit
once e fn l =
  liftEff $ runFn3 once_ e fn l

off
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → T.Evented
  → m Unit
off e l =
  liftEff $ runFn2 off_ e l

setIcon
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Icon
  → T.Marker
  → m T.Marker
setIcon i m =
  liftEff $ runFn2 setIcon_ i m

bindPopup
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → T.Layer
  → m T.Layer
bindPopup s l =
  liftEff $ runFn2 bindPopup_ s l

openPopup
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Maybe T.LatLng
  → T.Layer
  → m T.Layer
openPopup mbLL l = liftEff $ case mbLL of
  Nothing → runFn3 openPopup_ false [ ] l
  Just ll → runFn3 openPopup_ true (converter.convertLatLng ll) l

addLayer
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Layer
  → T.Leaflet
  → m T.Leaflet
addLayer lay leaf =
  liftEff $ runFn2 addLayer_ lay leaf

removeLayer
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Layer
  → T.Leaflet
  → m T.Leaflet
removeLayer lay leaf =
  liftEff $ runFn2 removeLayer_ lay leaf
