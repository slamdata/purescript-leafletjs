module Leaflet.Core.Converter where

import Prelude

import Color (Color, toHexString)

import Data.Foldable (intercalate)
import Data.URI (URIRef, printURIRef)

import Leaflet.Core.Types as T
import Leaflet.Util ((×), (∘))

type ConvertDict =
  { printURI ∷ URIRef → String
  , mkPoint ∷ T.Point → Array Int
  , printColor ∷ Color → String
  , convertLatLng ∷ T.LatLng → Array T.Degrees
  , printPercentOrPixel ∷ T.PercentOrPixel → String
  , printDashArray ∷ Array Int → String
  , printLineJoin ∷ T.LineJoin → String
  , printLineCap ∷ T.LineCap → String
  , printFillRule ∷ T.FillRule → String
  }

converter ∷ ConvertDict
converter =
  { printURI: printURIRef
  , mkPoint: \(a × b) → [a, b]
  , printColor: toHexString
  , convertLatLng: \{lat, lng} → [lat, lng]
  , printPercentOrPixel: case _ of
      T.Percent n → show n <> "%"
      T.Pixel i → show i
  , printDashArray: intercalate "," ∘ map show
  , printLineCap: case _ of
      T.ButtLC → "butt"
      T.RoundLC → "round"
      T.SquareLC → "square"
  , printLineJoin: case _ of
      T.MiterLJ → "miter"
      T.RoundLJ → "round"
      T.BevelLJ → "bevel"
  , printFillRule: case _ of
      T.NonZero → "nonzer"
      T.EvenOdd → "evenodd"
      T.Inherit → "inherit"
  }
