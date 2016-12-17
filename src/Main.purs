module Main where

import Prelude
import P1 as P1
import P2 as P2
import P3 as P3
import Control.Monad.Eff (Eff)

main :: Eff _ Unit
main = do
  P1.solve
  P2.test
  P3.test
