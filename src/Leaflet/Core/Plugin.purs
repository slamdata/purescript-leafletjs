module Leaflet.Core.Plugin
  ( onAddRemove
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, writeRef, readRef)

import Data.Function.Uncurried (Fn2, Fn4, runFn4, mkFn2)
import Data.Maybe (Maybe(..))

import DOM (DOM)

import Leaflet.Core.Types as T

foreign import onAddRemove_
  ∷ ∀ e
  . Fn4
      (Fn2 T.Layer T.Leaflet (Eff (dom ∷ DOM, ref ∷ REF|e) Unit))
      (Fn2 T.Layer T.Leaflet (Eff (dom ∷ DOM, ref ∷ REF|e) Unit))
      T.Layer
      T.Leaflet
      (Eff (dom ∷ DOM, ref ∷ REF|e) Unit)

onAddRemove
  ∷ ∀ e a m
  . MonadEff (dom ∷ DOM, ref ∷ REF|e) m
  ⇒ (T.Layer → T.Leaflet → Eff (dom ∷ DOM, ref ∷ REF|e) a)
  → (T.Layer → T.Leaflet → Maybe a → Eff (dom ∷ DOM, ref ∷ REF|e) Unit)
  → T.Layer
  → T.Leaflet
  → m (Ref (Maybe a))
onAddRemove init finish lay leaf = liftEff do
  ref ← newRef Nothing
  runFn4 onAddRemove_
    (mkFn2 \l lf → do
        res ← init l lf
        writeRef ref $ Just res)
    (mkFn2 \l lf → do
        mbv ← readRef ref
        finish l lf mbv)
    lay
    leaf
  pure ref
