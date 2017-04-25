module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)

import Debug.Trace as DT

main ∷ ∀ e.Eff e Unit
main = do
  DT.traceAnyA "Hey there, test"
