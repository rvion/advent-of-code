module P3 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Array (concat, filter, length)
import Data.Foldable (and)
import Data.Int (fromString)
import Data.String (Pattern(Pattern), split)
import Util (chunks, getFile, transpose, unsafeFromMaybe)

type Triangle = Array Int

test ::forall e. Eff _ Unit
test = do
  input <- getFile "src/p3.txt"
  logShow $ "917  == " <> show (solve input extract1)
  logShow $ "1649 == " <> show (solve input extract2)

solve :: String -> (String -> Array Triangle) -> Int
solve str extract = extract str
  # filter isValidTriangle
  # length

isValidTriangle :: Array Int -> Boolean
isValidTriangle = case _ of
  [a,b,c] -> and
    [ a+b > c
    , a+c > b
    , b+c > a]
  _ -> false

extract1 :: String -> Array Triangle
extract1 = parseTriangle

extract2 :: String -> Array Triangle
extract2 = parseTriangle
  >>> chunks 3
  >>> map transpose
  >>> concat

parseTriangle :: String -> Array Triangle
parseTriangle str = lines # map toTriangle
  where
    lines = split (Pattern "\n") str
    toTriangle = split (Pattern " ")
      >>> filter (_ /= "")
      >>> map (fromString >>> unsafeFromMaybe)
