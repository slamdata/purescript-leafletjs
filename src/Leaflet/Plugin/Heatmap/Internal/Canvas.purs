module Leaflet.Plugin.Heatmap.Internal.Canvas
  ( draw
  , createCanvas
  , canvasToElement
  , defaultOptions
  , HeatmapOptions
  , HeatmapPoint
  , module G
  ) where

import Prelude

import Color (Color)
import Color as Color

import Control.Monad.Eff (Eff, foreachE)

import Data.Array as A
import Data.ArrayBuffer.Types (Uint8ClampedArray)
import Data.Function.Uncurried (Fn2, Fn3, runFn3, runFn2, Fn4, runFn4)

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (HTMLDocument)
import DOM.Node.Types (Document, Element)
import DOM.Node.Document (createElement)

import Graphics.Canvas as G

import Math as Math

import Unsafe.Coerce (unsafeCoerce)

import Leaflet.Util ((∘))

foreign import modifyImageData ∷ Fn2 G.ImageData (Uint8ClampedArray → Uint8ClampedArray) G.ImageData
foreign import unsafeSet ∷ ∀ a. Fn3 Int a (Array a) (Array a)
foreign import unsafeGet ∷ ∀ a. Fn2 Int (Array a) a
foreign import unsafeFor ∷ ∀ a. Fn4 Int Int (Array a) (Int → Array a) (Array a)

asIntArray ∷ Uint8ClampedArray → Array Int
asIntArray = unsafeCoerce

fromIntArray ∷ Array Int → Uint8ClampedArray
fromIntArray = unsafeCoerce

createCanvas ∷ ∀ e. Eff (dom ∷ DOM|e) G.CanvasElement
createCanvas = do
  w ← window
  doc ← document w
  map elementToCanvas
    $ createElement "canvas"
    $ htmlDocumentToDocument doc
  where
  htmlDocumentToDocument ∷ HTMLDocument → Document
  htmlDocumentToDocument = unsafeCoerce

elementToCanvas ∷ Element → G.CanvasElement
elementToCanvas = unsafeCoerce

canvasToElement ∷ G.CanvasElement → Element
canvasToElement = unsafeCoerce

type HeatmapPoint = { x ∷ Number, y ∷ Number, i ∷ Number }

gradientData
  ∷ ∀ e
  . Array {color ∷ Color, stop ∷ Number }
  → Eff (canvas ∷ G.CANVAS, dom ∷ DOM|e) (Array Int)
gradientData stops = do
  canvas ← createCanvas
  _ ← G.setCanvasDimensions {width: 1.0, height: 256.0} canvas
  ctx ← G.getContext2D canvas
  gradient ← G.createLinearGradient {x0: 0.0, y0: 0.0, x1: 0.0, y1: 256.0}  ctx
  foreachE stops \{color, stop} → do
    void $ G.addColorStop stop (Color.cssStringRGBA color) gradient
  _ ← G.setGradientFillStyle gradient ctx
  _ ← G.fillRect ctx { x: 0.0, y: 0.0, h: 256.0, w: 1.0 }
  imgData ← G.getImageData ctx 0.0 0.0 1.0 256.0
  effPure $ asIntArray $ G.imageDataBuffer imgData

radius
  ∷ ∀ e
  . Number
  → Number
  → Eff (canvas ∷ G.CANVAS, dom ∷ DOM|e) G.CanvasImageSource
radius r blur = do
  canvas ← createCanvas
  _ ← G.setCanvasDimensions { width: dia, height: dia } canvas

  ctx ← G.getContext2D canvas
  _ ← G.setShadowOffsetX dia ctx
  _ ← G.setShadowOffsetY dia ctx
  _ ← G.setShadowBlur blur ctx
  _ ← G.setShadowColor "black" ctx
  _ ← G.beginPath ctx
  _ ← G.arc ctx {x: -1.0 * rad, y: -1.0 * rad, r: r, start: 0.0, end: Math.pi * 2.0 }
  _ ← G.closePath ctx
  _ ← G.fill ctx

  effPure $ G.canvasElementToImageSource canvas
  where
  rad = r + blur
  dia = rad + rad

draw
  ∷ ∀ e
  . G.CanvasElement
  → HeatmapOptions
  → Array HeatmapPoint
  → Eff (canvas ∷ G.CANVAS, dom ∷ DOM|e) Unit
draw canvas opts points = do
  gr ← gradientData opts.colorStops
  circle ← radius opts.radius opts.blur
  h ← G.getCanvasHeight canvas
  w ← G.getCanvasWidth canvas
  ctx ← G.getContext2D canvas
  _ ← G.clearRect ctx { x: 0.0, y: 0.0, h, w }
  foreachE points \{x, y, i} → do
    _ ← G.setGlobalAlpha ctx $ Math.max (i / opts.maxIntensity) opts.minOpacity
    _ ← G.drawImage ctx circle (x - opts.radius) (y - opts.radius)
    effUnit
  imgData ← G.getImageData ctx 0.0 0.0 w h
  let
    newImageData =
      runFn2 modifyImageData imgData
      $ fromIntArray
      ∘ colorize gr
      ∘ asIntArray
  void $ G.putImageData ctx newImageData 0.0 0.0

type HeatmapOptions =
  { minOpacity ∷ Number
  , maxIntensity ∷ Number
  , radius ∷ Number
  , blur ∷ Number
  , colorStops ∷ Array { stop ∷ Number, color ∷ Color }
  }

defaultOptions ∷ HeatmapOptions
defaultOptions =
  { minOpacity: 0.05
  , maxIntensity: 1.0
  , radius: 25.0
  , blur: 15.0
  , colorStops:
      [ { stop: 0.4, color: Color.rgba 0 0 255 1.0 }
      , { stop: 0.6, color: Color.rgba 0 255 255 1.0 }
      , { stop: 0.7, color: Color.rgba 0 255 0 1.0 }
      , { stop: 0.8, color: Color.rgba 255 255 0 1.0 }
      , { stop: 1.0, color: Color.rgba 255 0 0 1.0 }
      ]
  }

colorize ∷ Array Int → Array Int → Array Int
colorize grs circle =
  -- Using `circle` is totally unsafe after `colorize`
  runFn4 unsafeFor 0 (len - 1) circle \i →
    if i `mod` 4 /= 0
    then circle
    else
    let j = runFn2 unsafeGet (i + 3) circle
    in runFn3 unsafeSet i (runFn2 unsafeGet (j * 4) grs)
       $ runFn3 unsafeSet (i + 1) (runFn2 unsafeGet (j * 4 + 1) grs)
       $ runFn3 unsafeSet (i + 2) (runFn2 unsafeGet (j * 4 + 2) grs)
       $ circle
  where
  len ∷ Int
  len = A.length circle

effUnit ∷ ∀ e. Eff e Unit
effUnit = effPure unit

effPure ∷ ∀ e a. a → Eff e a
effPure = pure
