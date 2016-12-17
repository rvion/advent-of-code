module P3 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (filter, foldl, index, length, scanl, sort)
import Data.Foldable (and)
import Data.Int (fromString)
import Data.JSDate (isValid)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (Pattern(..), charAt, fromCharArray, split, toCharArray)
import Partial.Unsafe (unsafeCrashWith)
import Util (getFile)

test :: Eff _ Unit
test = do
  file <- getFile "src/p3.txt"
  logShow (solve1 file)

-- solve1 :: String -> Array (Array String)
-- solve1 :: String -> Array (Array Int)
solve1 :: String -> Int
solve1 str = filter isValid triangles # length
  where
    lines = split (Pattern "\n") str
    triangles = lines # map toTriangle
    toTriangle = split (Pattern " ")
      >>> filter (_ /= "")
      >>> map (fromString >>> unsafeFromMaybe)
      -- >>> sort

    isValid :: Array Int -> Boolean
    isValid = case _ of
      [a,b,c] -> and
        [ a+b > c
        , a+c > b
        , b+c > a]
      _ -> false

answer1 :: Int
answer1 = 917

unsafeFromMaybe :: forall a. Maybe a -> a
unsafeFromMaybe = case _ of
  Nothing -> unsafeCrashWith "maybe was not a just"
  Just a -> a
-- unsafeFromMaybe2 = fromMaybe (unsafeCrashWith "maybe was not a just 2")
