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

test :: Eff _ Unit
test = do
  input <- getFile "src/p3.txt"
  logShow $ "917 == " <> show (solve input extract1)
  logShow $ "??? == " <> show (solve input extract2)
  logShow $ [[1,2,3],[4,5,6],[7,8,9]] # chunks 3
    # map transpose
    # concat

extract1 :: String -> Array Triangle
extract1 str = lines # map toTriangle
  where
    lines = split (Pattern "\n") str
    toTriangle = split (Pattern " ")
      >>> filter (_ /= "")
      >>> map (fromString >>> unsafeFromMaybe)

extract2 :: String -> Array Triangle
extract2 str = extract1 str
  # chunks 3
  # map transpose
  # concat

solve :: String -> (String -> Array Triangle) -> Int
solve str extract = filter isValid (extract str) # length
  where
    isValid :: Array Int -> Boolean
    isValid = case _ of
      [a,b,c] -> and
        [ a+b > c
        , a+c > b
        , b+c > a]
      _ -> false

unsafeFromMaybe :: forall a. Maybe a -> a
unsafeFromMaybe = case _ of
  Nothing -> unsafeCrashWith "maybe was not a just"
  Just a -> a

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
