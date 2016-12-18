module P4 where

import Prelude
import Data.Array as A
import Data.NonEmpty as NE
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Array (filter, find, group, many, sort, sortBy, take)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern), Replacement(Replacement), contains, fromCharArray, replaceAll, split, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import ParseUtils (lowerCaseLetter, parseInt)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (ParseError, runParser)
import Text.Parsing.Parser.String (anyChar)
import Util (getFile)

test ::forall e. Eff _ Unit
test = do
  input <- getFile "src/p4.txt"
  let validRooms = solve input
  logShow $ "158835 == " <> show (solve1 validRooms)
  logShow $ "   993 == " <> show (solve2 validRooms)

solve :: String -> Array Room
solve str = str
  # split (Pattern "\n")
  # filter (_ /= "")
  # map parseRoom
  # filter isValid

solve1 :: Array Room -> Int
solve1 rooms = rooms
  # map (_.uid)
  # sum

solve2 :: Array Room -> Int
solve2 rooms = case mbRoom of
  Nothing -> unsafeCrashWith "no solution found"
  Just sol -> fst sol
  where
    mbRoom = rooms
      # map (\{uid,name} -> Tuple uid $ name
        # toCharArray
        # map (shift uid)
        # fromCharArray
        )
      # find (snd >>> contains (Pattern "northpoleobjectstorage"))

shift :: Int -> Char -> Char
shift x c = c
  # toCharCode
  # (_ - aCode)
  # (_ + x)
  # (_ `mod` 26)
  # add aCode
  # fromCharCode
  where
   aCode = toCharCode 'a'


isValid :: Room -> Boolean
isValid {realChecksum, givenChecksum} =
  realChecksum == givenChecksum

type Room = {
  uid :: Int,
  name :: String,
  nams :: String,
  givenChecksum:: String,
  realChecksum :: String
}

mkChecksum :: String -> String
mkChecksum str = str
  # toCharArray
  # sort
  # group
  # sortBy (\a b ->
      let
        la = A.length (A.fromFoldable b)
        lb = A.length (A.fromFoldable a)
      in
        if la == lb
        then compare (NE.head a) (NE.head b)
        else compare la lb)
  # take 5
  # map NE.head
  # fromCharArray

parseRoom :: String -> Room
parseRoom str = case eRoom of
  Left e -> unsafeCrashWith (show str)
  Right room -> room
  where
    eRoom :: Either ParseError Room
    eRoom = runParser (simplify str) do
      name <- fromCharArray <$> many lowerCaseLetter
      uid <- parseInt
      givenChecksum <- fromCharArray <$> many anyChar
      let realChecksum = mkChecksum name
      let nams = name # (toCharArray >>> sort >>> fromCharArray)
      pure { name, nams, uid, givenChecksum, realChecksum}

    simplify :: String -> String
    simplify =
      replaceAll (Pattern "-") (Replacement "") >>>
      replaceAll (Pattern "[") (Replacement "") >>>
      replaceAll (Pattern "]") (Replacement "")
