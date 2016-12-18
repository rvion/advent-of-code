module Main where

import Prelude
import P1 as P1
import P2 as P2
import P3 as P3
import P4 as P4
import P5 as P5

import Control.Monad.Eff (Eff)

main :: Eff _ Unit
main = do
  P1.solve
  P2.test
  P3.test
  P4.test
  P5.test
