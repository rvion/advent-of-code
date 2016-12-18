module P6 where

import Prelude
import Data.Array as A
import Data.NonEmpty as NE
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Array (group, head, sort, sortBy)
import Data.String (Pattern(Pattern), fromCharArray, split, toCharArray)
import Util (getFile, transpose, unsafeFromMaybe)
type Triangle = Array Int

data Q = Q1 | Q2
test ::forall e. Eff _ Unit
test = do
  input <- getFile "src/p6.txt"
  logShow $ "tzstqsua == " <> show (solve Q1 input)
  logShow $ "myregdnr == " <> show (solve Q2 input)

solve :: Q -> String -> String
solve q str = str
  # split (Pattern "\n")
  # map toCharArray
  # transpose
  # map (
    sort
    >>> group
    >>> sortBy (\a b ->
      let la = A.length (A.fromFoldable b)
          lb = A.length (A.fromFoldable a)
      in case q of
        Q1 -> compare la lb
        Q2 -> compare lb la)
    >>> head
    >>> unsafeFromMaybe
    >>> NE.head
    )
  # fromCharArray
