module Main where

import Prelude

import Color (Color, toHexString)
import Color as Color

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Plus (class Plus, empty)

import Data.Array as A
import Data.Either (Either(..))
import Data.Int as Int
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn6, mkFn2, mkFn3, runFn3, runFn6, runFn4, runFn2)
import Data.List as L
import Data.Traversable as F
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.StrMap as SM
import Data.Path.Pathy (file, dir, (</>), rootDir, currentDir)

import DOM (DOM)
import DOM.HTML (window)
import DOM.Node.Types (Element)
import DOM.Classy.ParentNode (querySelector)
import DOM.Classy.Element (class IsElement, toElement, setAttribute)
import DOM.Classy.ParentNode (class IsParentNode)
import DOM.Classy.Node (appendChild)
import DOM.HTML.Window (document)
import Data.URI (URIRef, printURIRef)
import Data.URI.Types as URI

import Graphics.Canvas (CANVAS)
import Graphics.Canvas as G

import Math as Math

import Debug.Trace as DT

import Leaflet.Types
import Leaflet.Heatmap
import Leaflet.Util ((∘))

import HeatmapData (testData)
import HeatmapLayerData (heatmapLayerData)
import Unsafe.Coerce (unsafeCoerce)

type ConvertDict =
  { printURI ∷ URIRef → String
  , mkPoint ∷ Point → Array Int
  , printColor ∷ Color → String
  , convertLatLng ∷ LatLng → Array Degrees
  , printPercentOrPixel ∷ PercentOrPixel → String
  , printDashArray ∷ Array Int → String
  , printLineJoin ∷ LineJoin → String
  , printLineCap ∷ LineCap → String
  , printFillRule ∷ FillRule → String
  }

foreign import debugTime ∷ ∀ e a. String → Eff e a → Eff e a

converter ∷ ConvertDict
converter =
  { printURI: printURIRef
  , mkPoint: \(Tuple a b) → [a, b]
  , printColor: toHexString
  , convertLatLng: \{lat, lng} → [lat, lng]
  , printPercentOrPixel: case _ of
      Percent n → show n <> "%"
      Pixel i → show i
  , printDashArray: F.intercalate "," ∘ map show
  , printLineCap: case _ of
      ButtLC → "butt"
      RoundLC → "round"
      SquareLC → "square"
  , printLineJoin: case _ of
      MiterLJ → "miter"
      RoundLJ → "round"
      BevelLJ → "bevel"
  , printFillRule: case _ of
      NonZero → "nonzer"
      EvenOdd → "evenodd"
      Inherit → "inherit"
  }

mkDegrees ∷ Number → Maybe Degrees
mkDegrees n
  | n > 360.0 || n < -360.0 = Nothing
  | otherwise = Just $ Degrees n


foreign import onload ∷ ∀ e a. Eff e a → Eff e a
foreign import map_ ∷ ∀ e. Element → Eff (dom ∷ DOM|e) Leaflet
foreign import setView_ ∷ ∀ e. Array Degrees → Leaflet → Eff (dom ∷ DOM|e) Leaflet
foreign import setZoom_ ∷ ∀ e. Int → Leaflet → Eff (dom ∷ DOM|e) Leaflet
foreign import tileLayer_ ∷ ∀ e. String → Eff (dom ∷ DOM|e) TileLayer
foreign import marker_ ∷ ∀ e. Array Degrees → Eff (dom ∷ DOM|e) Marker
foreign import addLayer_ ∷ ∀ e. Layer → Leaflet → Eff (dom ∷ DOM|e) Leaflet
foreign import removeLayer_ ∷ ∀ e. Layer → Leaflet → Eff (dom ∷ DOM|e) Leaflet
foreign import icon_ ∷ ∀ e r. ConvertDict → r → Eff (dom ∷ DOM|e)Icon
foreign import setIcon_ ∷ ∀ e. Icon → Marker → Eff (dom ∷ DOM|e) Marker
foreign import popup_ ∷ ∀ e r. ConvertDict → r → Eff (dom ∷ DOM|e) Popup
foreign import setLatLng_ ∷ ∀ e. Array Degrees → Popup → Eff (dom ∷ DOM|e) Popup
foreign import setContent_ ∷ ∀ e. String → Popup → Eff (dom ∷ DOM|e) Popup
foreign import openOn_ ∷ ∀ e. Leaflet → Popup → Eff (dom ∷ DOM|e) Popup
foreign import bindPopup_ ∷ ∀ e. String → Layer → Eff (dom ∷ DOM|e) Layer
foreign import openPopup_ ∷ ∀ e. Boolean → Array Degrees → Layer → Eff (dom ∷ DOM|e) Layer
foreign import circleMarker_ ∷ ∀ e r. Array Degrees → ConvertDict → r → Eff (dom ∷ DOM|e) CircleMarker
foreign import circle_ ∷ ∀ e r. Array Degrees → ConvertDict → r → Eff (dom ∷ DOM|e) Circle
foreign import polyline_ ∷ ∀ e r. Array (Array Degrees) → ConvertDict → r → Eff (dom ∷ DOM|e) Polyline
foreign import polygon_ ∷ ∀ e r. Array (Array Degrees) → ConvertDict → r → Eff (dom ∷ DOM|e) Polygon
foreign import rectangle_
  ∷ ∀ e r. Array (Array Degrees) → ConvertDict → r → Eff (dom ∷ DOM|e) Rectangle
foreign import layer_ ∷ ∀ e. Eff (dom ∷ DOM|e) Layer
foreign import on_
  ∷ ∀ e. String → (Event → Eff (dom ∷ DOM|e) Unit) → Evented → Eff (dom ∷ DOM|e) Unit
foreign import onAddRemove_
  ∷ ∀ e a
  . Fn6
      (Maybe a)
      (a → Maybe a)
      (Fn2 Layer Leaflet (Eff (dom ∷ DOM|e) a))
      (Fn3 Layer Leaflet (Maybe a) (Eff (dom ∷ DOM|e) Unit))
      Layer
      Leaflet
      (Eff (dom ∷ DOM, ref ∷ REF|e) (Ref (Maybe a)))
foreign import getSize_
  ∷ ∀ e. Fn2 (Int → Int → Point) Leaflet (Eff (dom ∷ DOM|e) Point)

foreign import any3d_
  ∷ ∀ e. Eff (dom ∷ DOM|e) Boolean

foreign import zoomAnimation_
  ∷ ∀ e. Leaflet → Eff (dom ∷ DOM|e) Boolean

foreign import getPanes_
  ∷ ∀ e. Leaflet → Eff (dom ∷ DOM|e) (SM.StrMap Element)

foreign import containerPointToLayerPoint_
  ∷ ∀ e. Fn3 (Int → Int → Point) (Array Int) Leaflet (Eff (dom ∷ DOM|e) Point)

foreign import latLngToContainerPoint_
  ∷ ∀ e. Fn3 (Int → Int → Point) (Array Degrees) Leaflet (Eff (dom ∷ DOM|e) Point)

foreign import eventZoom_
  ∷ ∀ e a. Fn3 (Maybe a) (a → Maybe a) Event (Eff (dom ∷ DOM|e) (Maybe Zoom))

foreign import eventCenter_
  ∷ ∀ e a. Fn4 (Maybe a) (a → Maybe a) (a → a → Tuple a a) Event (Eff (dom ∷ DOM|e) (Maybe Point))

foreign import getZoomScale_
  ∷ ∀ e. Zoom → Leaflet → Eff (dom ∷ DOM|e) Number

foreign import getMapPanePos_
  ∷ ∀ e a. Fn2 (a → a → Tuple a a) Leaflet (Eff (dom ∷ DOM|e) Point)

foreign import getCenterOffset_
  ∷ ∀ e a. Fn3 (a → a → Tuple a a) (Array Int) Leaflet (Eff (dom ∷ DOM|e) Point)

foreign import setTransform_
  ∷ ∀ e. Fn3 Element (Array Int) Number (Eff (dom ∷ DOM|e) Unit)

foreign import getMaxZoom_
  ∷ ∀ e. Leaflet → Eff (dom ∷ DOM|e) Zoom

foreign import getZoom_
  ∷ ∀ e. Leaflet → Eff (dom ∷ DOM|e) Zoom

--------------------------------------------------------------------------------
-- DOMUtil
--------------------------------------------------------------------------------
foreign import testProp_
  ∷ ∀ e a. Fn3 a (a → Maybe a) (Array String) (Eff (dom ∷ DOM|e) (Maybe String))
foreign import setStyle_
  ∷ ∀ e. Fn3 String String Element (Eff (dom ∷ DOM|e) Unit)
foreign import setPosition_
  ∷ ∀ e. Element → Array Int → Eff (dom ∷ DOM|e) Unit

getMaxZoom
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Leaflet
  → m Zoom
getMaxZoom = liftEff ∘ getMaxZoom_

getZoom
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Leaflet
  → m Zoom
getZoom = liftEff ∘ getZoom_

getMapPanePos
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Leaflet
  → m Point
getMapPanePos l =
  liftEff $ runFn2 getMapPanePos_ Tuple l

getCenterOffset
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Point
  → Leaflet
  → m Point
getCenterOffset (Tuple a b) l =
  liftEff $ runFn3 getCenterOffset_ Tuple [a, b] l

setTransform
  ∷ ∀ e m n
  . IsElement n
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ n
  → Point
  → Number
  → m Unit
setTransform n (Tuple a b) scale =
  liftEff $ runFn3 setTransform_ (toElement n) [a, b] scale

eventCenter
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Event
  → m (Maybe Point)
eventCenter e =
  liftEff $ runFn4 eventCenter_ Nothing Just Tuple e

eventZoom
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Event
  → m (Maybe Zoom)
eventZoom e =
  liftEff $ runFn3 eventZoom_ Nothing Just e

getZoomScale
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Zoom
  → Leaflet
  → m Number
getZoomScale zoom = liftEff ∘ getZoomScale_ zoom

setPosition
  ∷ ∀ m e n
  . MonadEff (dom ∷ DOM|e) m
  ⇒ IsElement n
  ⇒ n
  → Point
  → m Unit
setPosition n (Tuple a b) = liftEff $ setPosition_ (toElement n) [a, b]

containerPointToLayerPoint
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Point
  → Leaflet
  → m Point
containerPointToLayerPoint (Tuple a b) l =
  liftEff $ runFn3 containerPointToLayerPoint_ Tuple [a, b] l

latLngToContainerPoint
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ LatLng
  → Leaflet
  → m Point
latLngToContainerPoint {lat, lng} l =
  liftEff $ runFn3 latLngToContainerPoint_ Tuple [lat, lng] l

getPanes
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Leaflet
  → m (SM.StrMap Element)
getPanes = liftEff ∘ getPanes_

any3d
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ m Boolean
any3d = liftEff any3d_

zoomAnimation
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Leaflet
  → m Boolean
zoomAnimation = liftEff ∘ zoomAnimation_

getSize
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Leaflet
  → m Point
getSize l = liftEff $ runFn2 getSize_ Tuple l

setStyle
  ∷ ∀ e m n
  . IsElement n
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → String
  → n
  → m Unit
setStyle k v n = liftEff $ runFn3 setStyle_ k v $ toElement n

testProp
  ∷ ∀ e m f
  . MonadEff (dom ∷ DOM|e) m
  ⇒ F.Foldable f
  ⇒ f String
  → m (Maybe String)
testProp a =
  liftEff $ runFn3 testProp_ Nothing Just $ A.fromFoldable a

onAddRemove
  ∷ ∀ e a m
  . MonadEff (dom ∷ DOM, ref ∷ REF|e) m
  . (Layer → Leaflet → Eff (dom ∷ DOM|e) a)
  → (Layer → Leaflet → Maybe a → Eff (dom ∷ DOM|e) Unit)
  → Layer
  → Leaflet
  → m (Ref (Maybe a))
onAddRemove init finish l lf =
  runFn6 onAddRemove_ Nothing Just (mkFn2 init) (mkFn3 finish) l lf

setView ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ LatLng → Leaflet → m Leaflet
setView latLng = liftEff ∘ setView_ (converter.convertLatLng latLng)

setZoom ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ Int → Leaflet → m Leaflet
setZoom i = liftEff ∘ setZoom_ i

tileLayer ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ URIRef → m TileLayer
tileLayer = liftEff ∘ tileLayer_ ∘ converter.printURI

leaflet ∷ ∀ n e m. IsElement n ⇒ MonadEff (dom ∷ DOM|e) m ⇒ n → m Leaflet
leaflet = liftEff ∘ map_ ∘ toElement

marker ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ LatLng → m Marker
marker latLng = liftEff $ marker_ $ converter.convertLatLng latLng

addLayer ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ Layer → Leaflet → m Leaflet
addLayer l = liftEff ∘ addLayer_ l

removeLayer ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ Layer → Leaflet → m Leaflet
removeLayer l = liftEff ∘ removeLayer_ l

icon ∷ ∀ e r1 r2 m. Union r1 r2 IconConf ⇒ MonadEff (dom ∷ DOM|e) m ⇒ Record r1 → m Icon
icon = liftEff ∘ icon_ converter

setIcon ∷ ∀ m e. MonadEff (dom ∷ DOM|e) m ⇒ Icon → Marker → m Marker
setIcon i = liftEff ∘ setIcon_ i

popup ∷ ∀ e r1 r2 m. MonadEff (dom ∷ DOM|e) m ⇒ Union r1 r2 PopupConf ⇒ Record r1 → m Popup
popup = liftEff ∘ popup_ converter

circleMarker
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 CircleConf
  ⇒ LatLng
  → Record r1
  → m CircleMarker
circleMarker ll = liftEff ∘ circleMarker_ (converter.convertLatLng ll) converter

circle
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 CircleConf
  ⇒ LatLng
  → Record r1
  → m Circle
circle ll = liftEff ∘ circle_ (converter.convertLatLng ll) converter

polyline
  ∷ ∀ e r1 r2 m f
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (PolylineConf ())
  ⇒ F.Foldable f
  ⇒ f LatLng
  → Record r1
  → m Polyline
polyline lls =
  liftEff ∘ polyline_ (F.foldMap (A.singleton ∘ converter.convertLatLng) lls) converter

polygon
  ∷ ∀ e r1 r2 m f
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (PolylineConf ())
  ⇒ F.Foldable f
  ⇒ f LatLng
  → Record r1
  → m Polygon
polygon lls =
  liftEff ∘ polygon_ (F.foldMap (A.singleton ∘ converter.convertLatLng) lls) converter

rectangle
  ∷ ∀ e r1 r2 m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (PolylineConf ())
  ⇒ LatLng
  → LatLng
  → Record r1
  → m Rectangle
rectangle a b =
  liftEff ∘ rectangle_ [ converter.convertLatLng a, converter.convertLatLng b ] converter

layer ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ m Layer
layer = liftEff $ layer_

on ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ String → (Event → Eff (dom ∷ DOM|e) Unit) → Evented → m Unit
on e fn l = liftEff $ on_ e fn l

setContent ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ String → Popup → m Popup
setContent c = liftEff ∘ setContent_ c

openOn ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ Leaflet → Popup → m Popup
openOn m = liftEff ∘ openOn_ m

setLatLng ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ LatLng → Popup → m Popup
setLatLng latLng = liftEff ∘ setLatLng_ (converter.convertLatLng latLng)

bindPopup ∷ ∀ m e. MonadEff (dom ∷ DOM|e) m ⇒ String → Layer → m Layer
bindPopup s = liftEff ∘ bindPopup_ s

openPopup ∷ ∀ m e. MonadEff (dom ∷ DOM|e) m ⇒ Maybe LatLng → Layer → m Layer
openPopup mbLL = liftEff ∘ case mbLL of
  Nothing → openPopup_ false [ ]
  Just ll → openPopup_ true $ converter.convertLatLng ll

--------------------------------------------------------------------------------

mbLatLng ∷ Maybe LatLng
mbLatLng = do
  lat ← mkDegrees (-37.87)
  lng ← mkDegrees 175.457
  pure { lat, lng }

mkLatLng ∷ ∀ m. Plus m ⇒ Monad m ⇒ Number → Number → m LatLng
mkLatLng latN lngN = do
  lat ← maybe empty pure $ mkDegrees latN
  lng ← maybe empty pure $ mkDegrees lngN
  pure {lat, lng}

mkLatLngs ∷ ∀ e. MaybeT (Eff (dom ∷ DOM, random ∷ RANDOM|e)) (Array LatLng)
mkLatLngs = do
  let inp = A.range 0 5
  start ← maybe empty pure mbLatLng
  lats ← F.for inp \_ → do
    diff ← liftEff random
    maybe empty pure $ mkDegrees $ diff / 100.0 - 37.87
  lngs ← F.for inp \_ → do
    diff ← liftEff random
    maybe empty pure $ mkDegrees $ diff / 100.0 + 175.457
  pure $ A.zipWith (\lat lng → {lat, lng}) lats lngs


testURI ∷ URIRef
testURI =
  Left $ URI.URI
  (Just $ URI.URIScheme "http")
  (URI.HierarchicalPart
   (Just $ URI.Authority Nothing [Tuple (URI.NameAddress "{s}.tile.osm.org") Nothing])
   (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}.png"))
  Nothing
  Nothing

iconConf ∷ { iconUrl ∷ URIRef, iconSize ∷ Point }
iconConf =
  { iconUrl: Right $ URI.RelativeRef
      (URI.RelativePart Nothing $ Just $ Right $ currentDir </> file "marker.svg")
      Nothing
      Nothing
  , iconSize: Tuple 40 40
  }

heatmapOnAdd
  ∷ ∀ e f
  . F.Foldable f
  ⇒ HeatmapOptions
  → f { lat ∷ Degrees, lng ∷ Degrees, i ∷ Number }
  → Layer
  → Leaflet
  → Eff (dom ∷ DOM, canvas ∷ CANVAS|e) _
heatmapOnAdd opts items lay leaf = do


multiplyPoint ∷ Point → Number → Point
multiplyPoint (Tuple a b) scale
  | scale < 0.0 =
      Tuple (Int.floor $ Int.toNumber a / scale) (Int.floor $ Int.toNumber b / scale)
  | otherwise =
      Tuple (Int.floor $ Int.toNumber a * scale) (Int.floor $ Int.toNumber b * scale)

subtractPoint ∷ Point → Point → Point
subtractPoint (Tuple x1 y1) (Tuple x2 y2) = Tuple (x1 - x2) (y1 - y2)

addPoint ∷ Point → Point → Point
addPoint (Tuple x1 y1) (Tuple x2 y2) = Tuple (x1 + x2) (y1 + y2)

contains ∷ Bounds → Point → Boolean
contains (Tuple (Tuple x1 y1) (Tuple x2 y2)) (Tuple x y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2

heatmapOnRemove ∷ ∀ e. Layer → Leaflet → Maybe _ → Eff (dom ∷ DOM|e) Unit
heatmapOnRemove lay leaf state = do
  pure unit

basic ∷ ∀ e n. IsParentNode n ⇒ n → MaybeT (Eff (dom ∷ DOM, random ∷ RANDOM|e)) Unit
basic doc = void do
  el ← MaybeT $ querySelector (wrap "#map") doc
  tiles ← tileLayer testURI
  latLng ← maybe empty pure mbLatLng
  i ← icon iconConf
  m ← marker latLng >>= setIcon i
  let mL = markerToLayer m
  _ ← bindPopup "FOO!" mL >>= openPopup Nothing
  plLLs ← mkLatLngs
  pl ← polyline plLLs { color: Color.rgb 255 215 0 }
  pgLLs ← mkLatLngs
  pg ← polygon pgLLs { color: Color.rgb 0 0 0 }
  cM ← circleMarker latLng {radius: 64.0}
  c ← circle latLng {radius: 48.0, color: Color.rgb 0 255 0 }
  r1 ← mkLatLng (-37.27) (175.56)
  r2 ← mkLatLng (-37.18) (176.00)
  r ← rectangle r1 r2 { }
  leaflet el
    >>= setView latLng
    >>= setZoom 12
    >>= addLayer (tileToLayer tiles)
    >>= addLayer mL
    >>= addLayer (polylineToLayer pl)
    >>= addLayer (polygonToLayer pg)
    >>= addLayer (circleMarkerToLayer cM)
    >>= addLayer (circleToLayer c)
    >>= addLayer (rectangleToLayer r)

leafletHeatmap
  ∷ ∀ e n
  . IsParentNode n
  ⇒ n
  → MaybeT (Eff (ref ∷ REF, dom ∷ DOM, random ∷ RANDOM, canvas ∷ CANVAS|e)) Unit
leafletHeatmap doc = void do
  el ← MaybeT $ querySelector (wrap "#heatmap-leaflet") doc
  view ← mkLatLng (-37.87) (175.457)

  lay ← layer
  on "add" (\e → DT.traceAnyA "on Add") (layerToEvented lay)
  layState ←
    liftEff $ onAddRemove (heatmapOnAdd defaultOptions heatmapLayerData) heatmapOnRemove lay leaf

  tiles ← tileLayer testURI
  _ ← leaflet el
    >>= setView view
    >>= setZoom 12
    >>= addLayer (tileToLayer tiles)
    >>= addLayer lay
    >>= removeLayer lay
  pure unit

main ∷ ∀ e.Eff (ref ∷ REF, canvas ∷ CANVAS, dom ∷ DOM, random ∷ RANDOM|e) Unit
main = onload do
  doc ← window >>= document
  void $ runMaybeT do
    basic doc
    leafletHeatmap doc
