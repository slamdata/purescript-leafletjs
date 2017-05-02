module Leaflet.Core.Units where

import Prelude

import Control.Plus (empty)
import Control.Alternative (class Alternative)

import Data.Int as Int

import Leaflet.Core.Types as T
import Leaflet.Util ((×))

import Unsafe.Coerce (unsafeCoerce)

mkZoom ∷ ∀ m. Alternative m ⇒ Int → m T.Zoom
mkZoom n
  | n < 0 = empty
  | otherwise = pure $ unsafeCoerce n

mkDegrees ∷ ∀ m. Alternative m ⇒ Number → m T.Degrees
mkDegrees n
  | n > 360.0 || n < -360.0 = empty
  | otherwise = pure $ unsafeCoerce n

mkLatLng ∷ ∀ m. Alternative m ⇒ Number → Number → m T.LatLng
mkLatLng lat lng =
  { lat: _
  , lng: _
  }
  <$> (mkDegrees lat)
  <*> (mkDegrees lng)

scalePoint ∷ T.Point → Number → T.Point
scalePoint (a × b) scale
  | scale < 0.0 =
    (Int.floor $ Int.toNumber a / scale) × (Int.floor $ Int.toNumber b / scale)
  | otherwise =
    (Int.floor $ Int.toNumber a * scale) × (Int.floor $ Int.toNumber b * scale)

subtractPoint ∷ T.Point → T.Point → T.Point
subtractPoint (x1 × y1) (x2 × y2) = (x1 - x2) × (y1 - y2)

addPoint ∷ T.Point → T.Point → T.Point
addPoint (x1 × y1) (x2 × y2) = (x1 + x2) × (y1 + y2)

contains ∷ T.Bounds → T.Point → Boolean
contains ((x1 × y1) × (x2 × y2)) (x × y) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2
