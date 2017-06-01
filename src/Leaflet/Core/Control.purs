module Leaflet.Core.Control
  ( layers
  , addTo
  , remove
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.StrMap as SM
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)

import DOM (DOM)

import Leaflet.Core.Types as T

foreign import layers_
  ∷ ∀ e r. Fn3 (SM.StrMap T.Layer) (SM.StrMap T.LayerGroup) r (Eff (dom ∷ DOM|e) T.Control)

foreign import addTo_
  ∷ ∀ e. Fn2 T.Leaflet T.Control (Eff (dom ∷ DOM|e) T.Control)

foreign import remove_
  ∷ ∀ e. T.Control → Eff (dom ∷ DOM|e) Unit

layers
  ∷ ∀ m e r1 r2
  . MonadEff (dom ∷ DOM|e) m
  ⇒ Union r1 r2 (T.LayerControlConf ())
  ⇒ SM.StrMap T.Layer
  → SM.StrMap T.LayerGroup
  → Record r1
  → m T.Control
layers bs os r =
  liftEff $ runFn3 layers_ bs os r

addTo
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Leaflet
  → T.Control
  → m T.Control
addTo leaf control =
  liftEff $ runFn2 addTo_ leaf control

remove
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM|e) m
  ⇒ T.Control
  → m Unit
remove control =
  liftEff $ remove_ control
