module Leaflet.Core.Event
  ( eventCenter
  , eventZoom
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import DOM (DOM)

import Leaflet.Util (type (×))
import Leaflet.Core.Types (Point, Zoom, Event)


foreign import eventZoom_
  ∷ ∀ e a. Fn3 (Maybe a) (a → Maybe a) Event (Eff (dom ∷ DOM|e) (Maybe Zoom))

foreign import eventCenter_
  ∷ ∀ e a. Fn4 (Maybe a) (a → Maybe a) (a → a → a × a) Event (Eff (dom ∷ DOM|e) (Maybe Point))

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
