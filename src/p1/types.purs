module P1.Types where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)

type Pos = {x :: Int, y :: Int}

data Foo = Foo {x :: Int, y :: Int}
derive instance genfoo :: Generic Foo
instance showFoo :: Show Foo where show = gShow

data Instruction = L Int | R Int
derive instance igen :: Generic Instruction
instance ishow :: Show Instruction where show = gShow

data Dir = N | S | W | E

type State = {
  pos :: Pos,
  dir :: Dir,
  visited :: Array Pos,
  firstCross :: Maybe Pos
  }
