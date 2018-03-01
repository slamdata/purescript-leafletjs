module Leaflet.Core.Converter where

import Prelude

import Color (Color, toHexString)
import Data.Foldable (intercalate)
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Leaflet.Core.Types (LeafURIRef)
import Leaflet.Core.Types as T
import Leaflet.Util ((×), (∘))
import URI.URIRef as URIRef

type ConvertDict =
  { printURI ∷ LeafURIRef → String
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
  { printURI: \uri →
     let
      oRx = URX.unsafeRegex "%7B" RXF.global
      cRx = URX.unsafeRegex "%7D" RXF.global
      encodedString = T.runLeafURIRef (\r -> URIRef.print r.opts r.uri) uri
      replaced = RX.replace oRx "{" $ RX.replace cRx "}" encodedString
     in replaced
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
