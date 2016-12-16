module Main where

import Prelude
import P1 as P1
import P2 as P2
import Control.Monad.Eff (Eff)

main :: Eff _ Unit
main = do
  P1.solve
  P2.main
