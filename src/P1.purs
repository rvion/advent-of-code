module P1 where

import Prelude
import Control.Alternative (empty, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Data.Array (any, findMap, range, foldl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import P1.Parer (parse)
import P1.Types (Dir(..), Foo(..), Instruction(..), Pos, State)
import Util (getFile)

solve :: Eff _ Unit
solve = do
  input <- parse <$> getFile "src/p1/input.txt"
  case input of
    Left err -> do
      log "error parsing"
      log (show err)
    Right x -> do
      let solution = reduce x
      log "success"
      logShow $ Foo solution.pos
      logShow $ Foo <$> solution.firstCross


reduce :: Array Instruction -> State
reduce = foldl move {
  pos: {x:0,y:0},
  dir: N,
  visited: empty,
  firstCross: Nothing
  }

move :: State -> Instruction -> State
move s instruction = s {
    pos = {
      x: s.pos.x + amount * dir.x,
      y: s.pos.y + amount * dir.y
    },
    dir = nextDir,
    visited = s.visited <> intermediatePos,
    firstCross = s.firstCross <|> findMap (isCross s.visited) intermediatePos
    }
  where
    amount = case instruction of
      L i -> i
      R i -> i
    dir = case nextDir of
      N -> {x:  0, y:  1}
      S -> {x:  0, y: -1}
      E -> {x:  1, y:  0}
      W -> {x: -1, y:  0}
    intermediatePos = map
      (\j-> {
        x: s.pos.x + j * dir.x,
        y: s.pos.y + j * dir.y
      })
      (range 1 amount)
    nextDir = case s.dir, instruction of
      N, L _ -> W
      N, R _ -> E
      S, L _ -> E
      S, R _ -> W
      W, L _ -> S
      W, R _ -> N
      E, L _ -> N
      E, R _ -> S

isCross :: Array Pos -> Pos -> Maybe Pos
isCross visited pos =
  if any (\p -> p.x == pos.x && p.y == pos.y) visited
  then Just pos
  else Nothing
