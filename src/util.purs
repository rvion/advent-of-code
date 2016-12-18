module Util where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (concat, cons, drop, take, uncons)
import Data.Maybe (Maybe(..))
import Node.Buffer (BUFFER, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readFile)
import Partial.Unsafe (unsafeCrashWith)

foreign import debug :: forall eff a.
  a -> Eff eff Unit

foreign import md5 :: String -> String

type ReadFileEffets eff =
  ( buffer :: BUFFER
  , fs :: FS
  , err :: EXCEPTION
  | eff
  )

getFile :: forall eff.
  String -> Eff (ReadFileEffets eff) String
getFile str = do
  buf <- readFile str
  toString UTF8 buf

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
