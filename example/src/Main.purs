module Main where

import Prelude

import Color (Color, toHexString)
import Color as Color

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Plus (class Plus, empty)

import Data.Array as A
import Data.Either (Either(..))
import Data.Traversable as F
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Data.Path.Pathy (file, dir, (</>), rootDir, currentDir)

import DOM (DOM)
import DOM.HTML (window)
import DOM.Node.Types (Element)
import DOM.Classy.ParentNode (querySelector)
import DOM.Classy.Element (class IsElement, toElement)
import DOM.Classy.ParentNode (class IsParentNode)
import DOM.HTML.Window (document)
import Data.URI (URIRef, printURIRef)
import Data.URI.Types as URI

import Graphics.Canvas (CANVAS)

import Debug.Trace as DT

import Leaflet.Types
import Leaflet.Heatmap (defaultOptions, draw, elementToCanvas)
import Leaflet.Util ((∘))

import HeatmapData (testData)

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
foreign import on_ ∷ ∀ e. String → (Layer → Eff (dom ∷ DOM|e) Unit) → Layer → Eff (dom ∷ DOM|e) Layer
foreign import onAddRemove
  ∷ ∀ e a
  . (Layer → Leaflet → Eff (dom ∷ DOM|e) a)
  → (Layer → Leaflet → a → Eff (dom ∷ DOM|e) Unit)
  → Layer
  → Eff (dom ∷ DOM, ref ∷ REF|e) (Ref a)

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

on ∷ ∀ e m. MonadEff (dom ∷ DOM|e) m ⇒ String → (Layer → Eff (dom ∷ DOM|e) Unit) → Layer → m Layer
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

heatmap
  ∷ ∀ e n
  . IsParentNode n
  ⇒ n
  → MaybeT (Eff (canvas ∷ CANVAS, dom ∷ DOM, random ∷ RANDOM|e)) Unit
heatmap doc = void do
  el ← map elementToCanvas $ MaybeT $ querySelector (wrap "canvas#heatmap") doc
  liftEff $ debugTime "heatmap" $ draw el defaultOptions{maxIntensity = 18.0} testData
  pure unit

leafletHeatmap
  ∷ ∀ e n. IsParentNode n ⇒ n → MaybeT (Eff (ref ∷ REF, dom ∷ DOM, random ∷ RANDOM|e)) Unit
leafletHeatmap doc = void do
  el ← MaybeT $ querySelector (wrap "#heatmap-leaflet") doc
  view ← mkLatLng (-37.87) (175.457)

  lay ← layer >>= on "add" \l → DT.traceAnyA "on Add"
  layState ← liftEff $ onAddRemove
        (\l m → DT.traceAnyA "on add" *> pure 1)
        (\l m i → DT.traceAnyA "on remove" *> DT.traceAnyA i)
        lay

  tiles ← tileLayer testURI
  _ ← leaflet el
    >>= setView view
    >>= setZoom 12
    >>= addLayer (tileToLayer tiles)
    >>= addLayer lay
    >>= \l → liftEff do
      a ← readRef layState
      DT.traceAnyA "after add"
      DT.traceAnyA a
      pure l
    >>= \lc → liftEff do
      writeRef layState 12
      pure lc
    >>= removeLayer lay
    >>= \ll → liftEff do
      a ← readRef layState
      DT.traceAnyA layState
      DT.traceAnyA "after remove"
      pure ll
  pure unit

main ∷ ∀ e.Eff (ref ∷ REF, canvas ∷ CANVAS, dom ∷ DOM, random ∷ RANDOM|e) Unit
main = onload do
  doc ← window >>= document
  void $ runMaybeT do
--    basic doc
--    heatmap doc
    leafletHeatmap doc
