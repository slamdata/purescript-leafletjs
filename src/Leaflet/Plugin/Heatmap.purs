module Leaflet.Plugin.Heatmap
  ( mkHeatmap
  , module C
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (Ref, REF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

import Data.Array as A
import Data.Foldable (class Foldable, for_, intercalate)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (fst, snd)

import DOM (DOM)
import DOM.Classy.Element (setAttribute)
import DOM.Classy.Node (appendChild)

import Leaflet.Core as LC
import Leaflet.Plugin.Heatmap.Internal.Canvas as C
import Leaflet.Util ((∘), (×), type (×))

import Math as Math

mkHeatmap
  ∷ ∀ e m f
  . MonadEff (dom ∷ DOM, ref ∷ REF, canvas ∷ C.CANVAS|e) m
  ⇒ Foldable f
  ⇒ C.HeatmapOptions
  → f { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
  → LC.Layer
  → LC.Leaflet
  → m (Ref (Maybe Unit))
mkHeatmap opts items lay leaf = do
  LC.onAddRemove (onAdd opts items) onRemove lay leaf

onRemove ∷ ∀ e. LC.Layer → LC.Leaflet → Maybe Unit → Eff e Unit
onRemove _ _ _ = pure unit

onAdd
  ∷ ∀ e f
  . Foldable f
  ⇒ C.HeatmapOptions
  → f { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
  → LC.Layer
  → LC.Leaflet
  → Eff (dom ∷ DOM, canvas ∷ C.CANVAS|e) Unit
onAdd opts items lay leaf = do
  originProp ←
    LC.testProp [ "transformOrigin", "WebkitTransformOrigin", "msTransformOrigin" ]

  canvas ← C.createCanvas
  let
    canvasEl = C.canvasToElement canvas

  for_ originProp \p →
    LC.setStyle p "50% 50%" canvasEl

  x × y ← LC.getSize leaf
  _ ← C.setCanvasWidth (Int.toNumber x) canvas
  _ ← C.setCanvasHeight (Int.toNumber y) canvas

  threeD ← LC.any3d
  isZoom ← LC.zoomAnimation leaf
  let
    animClass
      | threeD && isZoom = "leaflet-zoom-animated"
      | otherwise = "leaflet-zoom-hide"

  setAttribute "class"
    (intercalate " " [ "leaflet-ps-heatmap-layer", "leaflet-layer", animClass ])
    canvasEl

  panes ← LC.getPanes leaf
  for_ (SM.lookup "overlayPane" panes) $ appendChild canvasEl

  let
    reset _ = do
      topLeft ← LC.containerPointToLayerPoint (0 × 0) leaf
      mapSize ← LC.getSize leaf
      _ ← C.setCanvasWidth (Int.toNumber x) canvas
      _ ← C.setCanvasHeight (Int.toNumber y) canvas
      LC.setPosition canvasEl topLeft
      redraw canvas items opts leaf

    zoomAnim e = void $ runMaybeT do
      zoom ← MaybeT $ LC.eventZoom e
      center ← MaybeT $ LC.eventCenter e
      scale ← LC.getZoomScale zoom leaf
      offset ← LC.getCenterOffset center leaf
      panePos ← LC.getMapPanePos leaf
      let coord = offset `LC.scalePoint` (-scale) # flip LC.subtractPoint panePos
      LC.setTransform canvasEl offset scale

  when (threeD && isZoom) $ LC.mapToEvented leaf # LC.on "zoomanim" zoomAnim

  LC.mapToEvented leaf # LC.on "moveend" reset

  redraw canvas items opts leaf

redraw
  ∷ ∀ e f
  . Foldable f
  ⇒ C.CanvasElement
  → f { lng ∷ LC.Degrees, lat ∷ LC.Degrees, i ∷ Number }
  → C.HeatmapOptions
  → LC.Leaflet
  → Eff (dom ∷ DOM, canvas ∷ C.CANVAS|e) Unit
redraw el items opts leaf = do
  size ← LC.getSize leaf
  maxZoom ← map LC.zoomToNumber $ LC.getMaxZoom leaf
  zoom ← map LC.zoomToNumber $ LC.getZoom leaf
  panePos ← LC.getMapPanePos leaf

  let
    radius ∷ Int
    radius = Int.floor opts.radius

    bounds ∷ LC.Bounds
    bounds = ((-radius) × (-radius)) × (LC.addPoint (radius × radius) size)

    intensityMultiplier ∷ Number
    intensityMultiplier =
      1.0 / (Math.pow 2.0 $ Math.max 0.0 $ Math.min 12.0 $ maxZoom - zoom)

    cellSize ∷ Int
    cellSize = radius / 2

    offsetX ∷ Int
    offsetX = (fst panePos) `mod` cellSize

    offsetY ∷ Int
    offsetY = (snd panePos) `mod` cellSize

    alterFn r Nothing = Just r
    alterFn rr (Just r) =
      let newI = rr.i + r.i
          newX = (r.x * r.i + rr.x * rr.i) / newI
          newY = (r.y * r.i + rr.y * rr.i) / newI
      in Just { x: newX, y: newY, i: newI }

    foldFn
      ∷ Map.Map (Int × Int) { x ∷ Number, y ∷ Number, i ∷ Number }
      → { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
      → Eff (dom ∷ DOM, canvas ∷ C.CANVAS|e)
          (Map.Map (Int × Int) { x ∷ Number, y ∷ Number, i ∷ Number })
    foldFn acc { lat, lng, i} = do
      p@(px × py) ← LC.latLngToContainerPoint {lat, lng} leaf
      if not $ LC.contains bounds p
        then pure acc
        else do
        let
          gx = (px - offsetX) / cellSize + 2
          gy = (py - offsetY) / cellSize + 2
          item = { x: Int.toNumber px, y: Int.toNumber py, i: i * intensityMultiplier }
        pure $ Map.alter (alterFn item) (gx × gy) acc

    groupPoints
      ∷ Array { lat ∷ LC.Degrees, lng ∷ LC.Degrees, i ∷ Number }
      → Eff (dom ∷ DOM, canvas ∷ C.CANVAS|e) (Array { x ∷ Number, y ∷ Number, i ∷ Number })
    groupPoints is =
      map (A.fromFoldable ∘ Map.values) $ A.foldRecM foldFn Map.empty is

    adjustPoints
      ∷ { x ∷ Number, y ∷ Number, i ∷ Number }
      → { x ∷ Number, y ∷ Number, i ∷ Number }
    adjustPoints {x, y, i} =
      { x: x - opts.radius / 2.0
      , y: y - opts.radius / 2.0
      , i: Math.min i opts.maxIntensity
      }

  grouppedPoints ← groupPoints $ A.fromFoldable items

  liftEff
    $ C.draw el opts
    $ map adjustPoints
    $ grouppedPoints
