module Main where

import Prelude

import Color as Color
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM (DOM)
import DOM.Classy.ParentNode (class IsParentNode, querySelector)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String.NonEmpty as NES
import Data.These (These(..))
import Data.Traversable as F
import Data.Tuple (Tuple(..))
import Graphics.Canvas (CANVAS)
import HeatmapLayerData (heatmapLayerData)
import Leaflet.Core (mkLeafURIRef)
import Leaflet.Core as LC
import Leaflet.Core.Types (LeafURIRef)
import Leaflet.Plugin.Heatmap as LH
import Leaflet.Util ((×))
import Partial.Unsafe (unsafePartial)
import URI (Authority(..), Fragment, HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), Port, Query, RelPath, RelativePart(..), RelativeRef(..), URI(..), URIRef, UserInfo)
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Path.Segment (segmentNZFromString)
import URI.Path.Segment as PathSegment
import URI.Scheme as Scheme
import URI.URIRef (URIRefOptions)

foreign import onload ∷ ∀ e a. Eff e a → Eff e a

type MainURIRef = URIRef UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment

mainURIRefOptions ∷ Record (URIRefOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
mainURIRefOptions =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseRelPath: pure
  , printRelPath: id
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }

mkLatLngs ∷ ∀ e. MaybeT (Eff (dom ∷ DOM, random ∷ RANDOM|e)) (Array LC.LatLng)
mkLatLngs = do
  let inp = A.range 0 5
  start ← LC.mkLatLng (-37.87) 175.457
  lats ← F.for inp \_ → do
    diff ← liftEff random
    LC.mkDegrees $ diff / 100.0 - 37.87
  lngs ← F.for inp \_ → do
    diff ← liftEff random
    LC.mkDegrees $ diff / 100.0 + 175.457
  pure $ A.zipWith (\lat lng → {lat, lng}) lats lngs


testURI ∷ LeafURIRef
testURI = mkLeafURIRef
  { uri: Left $ URI
      (Scheme.unsafeFromString "http")
      (HierarchicalPartAuth
      (Authority Nothing (Just $ This $ NameAddress $ RegName.fromString $ unsafePartial $ NES.unsafeFromString "{s}.tile.osm.org"))
      (Path $ map PathSegment.segmentFromString ["{z}", "{x}", "{y}.png"]))
      Nothing
      Nothing
  , opts: mainURIRefOptions
  }

iconConf ∷ { iconUrl ∷ LeafURIRef, iconSize ∷ LC.Point }
iconConf =
  { iconUrl: mkLeafURIRef
      { uri: Right $ RelativeRef
          (RelativePartNoAuth $ Just $ Left $ PathAbsolute $ Just $ Tuple (segmentNZFromString $ unsafePartial $ NES.unsafeFromString "marker.svg") [])
          Nothing
          Nothing
      , opts: mainURIRefOptions
      }
  , iconSize: 40 × 40
  }
core
  ∷ ∀ e n
  . IsParentNode n
  ⇒ n
  → MaybeT (Eff (dom ∷ DOM, random ∷ RANDOM|e)) Unit
core doc = void do
  el ← MaybeT $ querySelector (wrap "#map") doc
  tiles ← LC.tileLayer testURI
  latLng ← LC.mkLatLng (-37.87) 175.457
  i ← LC.icon iconConf
  m ← LC.marker latLng >>= LC.setIcon i
  let mL = LC.markerToLayer m
  _ ← LC.bindPopup "Hey! I'm a marker" mL >>= LC.openPopup Nothing
  plLLs ← mkLatLngs
  pl ← LC.polyline plLLs { color: Color.rgb 255 215 0 }
  pgLLs ← mkLatLngs
  pg ← LC.polygon pgLLs { color: Color.rgb 0 0 0 }
  cM ← LC.circleMarker latLng {radius: 64.0}
  c ← LC.circle latLng {radius: 48.0, color: Color.rgb 0 255 0 }
  r1 ← LC.mkLatLng (-37.27) (175.56)
  r2 ← LC.mkLatLng (-37.18) (176.00)
  r ← LC.rectangle r1 r2 { }
  zoom ← LC.mkZoom 12
  LC.leaflet el
    >>= LC.setView latLng
    >>= LC.setZoom zoom
    >>= LC.addLayer (LC.tileToLayer tiles)
    >>= LC.addLayer mL
    >>= LC.addLayer (LC.polylineToLayer pl)
    >>= LC.addLayer (LC.polygonToLayer pg)
    >>= LC.addLayer (LC.circleMarkerToLayer cM)
    >>= LC.addLayer (LC.circleToLayer c)
    >>= LC.addLayer (LC.rectangleToLayer r)

heatmap
  ∷ ∀ e n
  . IsParentNode n
  ⇒ n
  → MaybeT (Eff (ref ∷ REF, dom ∷ DOM, random ∷ RANDOM, canvas ∷ CANVAS|e)) Unit
heatmap doc = void do
  el ← MaybeT $ querySelector (wrap "#heatmap-leaflet") doc
  view ← LC.mkLatLng (-37.87) (175.457)
  leaf ← LC.leaflet el
  lay ← LC.layer
  layState ← LH.mkHeatmap LH.defaultOptions heatmapLayerData lay leaf
  tiles ← LC.tileLayer testURI
  zoom ← LC.mkZoom 12
  LC.setView view leaf
    >>= LC.setZoom zoom
    >>= LC.addLayer (LC.tileToLayer tiles)
    >>= LC.addLayer lay
    >>= LC.removeLayer lay

main ∷ ∀ e.Eff (ref ∷ REF, canvas ∷ CANVAS, dom ∷ DOM, random ∷ RANDOM|e) Unit
main = onload do
  doc ← window >>= document
  void $ runMaybeT do
    core doc
    heatmap doc
