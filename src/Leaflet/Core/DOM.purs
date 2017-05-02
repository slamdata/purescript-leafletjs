module Leaflet.Core.DOM
  ( testProp
  , setStyle
  , any3d
  , setPosition
  , setTransform
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn3, Fn2, runFn3, runFn2)

import DOM (DOM)
import DOM.Node.Types (Element)
import DOM.Classy.Element (class IsElement, toElement)

import Leaflet.Core.Types (Point)
import Leaflet.Util ((×))

foreign import any3d_
  ∷ ∀ e. Eff (dom ∷ DOM|e) Boolean

foreign import testProp_
  ∷ ∀ e a. Fn3 a (a → Maybe a) (Array String) (Eff (dom ∷ DOM|e) (Maybe String))

foreign import setStyle_
  ∷ ∀ e. Fn3 String String Element (Eff (dom ∷ DOM|e) Unit)

foreign import setPosition_
  ∷ ∀ e. Fn2 Element (Array Int) (Eff (dom ∷ DOM|e) Unit)

foreign import setTransform_
  ∷ ∀ e. Fn3 Element (Array Int) Number (Eff (dom ∷ DOM|e) Unit)

testProp
  ∷ ∀ e m f
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Foldable f
  ⇒ f String
  → m (Maybe String)
testProp a =
  liftEff $ runFn3 testProp_ Nothing Just $ A.fromFoldable a

setStyle
  ∷ ∀ e m n
  . IsElement n
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ String
  → String
  → n
  → m Unit
setStyle k v n = liftEff $ runFn3 setStyle_ k v $ toElement n

any3d
  ∷ ∀ e m
  . MonadEff (dom ∷ DOM|e) m
  ⇒ m Boolean
any3d = liftEff any3d_

setPosition
  ∷ ∀ m e n
  . MonadEff (dom ∷ DOM|e) m
  ⇒ IsElement n
  ⇒ n
  → Point
  → m Unit
setPosition n (a × b) =
  liftEff $ runFn2 setPosition_ (toElement n) [a, b]

setTransform
  ∷ ∀ e m n
  . IsElement n
  ⇒ MonadEff (dom ∷ DOM|e) m
  ⇒ n
  → Point
  → Number
  → m Unit
setTransform n (a × b) scale =
  liftEff $ runFn3 setTransform_ (toElement n) [a, b] scale
