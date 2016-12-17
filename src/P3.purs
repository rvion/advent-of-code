module P3 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Array (concat, cons, drop, filter, length, take, uncons)
import Data.Foldable (and)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (Pattern(Pattern), split)
import Partial.Unsafe (unsafeCrashWith)
import Util (getFile)

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

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = cons (take n xs) (chunks n $ drop n xs)

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose m = case uncons m of
  Just {head} -> case uncons head of
    Just val -> cons
      (map (take 1) m # concat)
      (transpose (m # map (drop 1)))
    _ -> []
  _ -> []

unsafeFromMaybe :: forall a. Maybe a -> a
unsafeFromMaybe = case _ of
  Nothing -> unsafeCrashWith "maybe was not a just"
  Just a -> a
