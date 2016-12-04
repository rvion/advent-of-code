module Util where

import Prelude
import Control.Alternative (empty, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (any, findMap, range, foldl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Buffer (BUFFER, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readFile)
import P1.Parer (parse)
import P1.Types (Dir(..), Foo(..), Instruction(..), Pos, State)

type ReadFileEffets eff =
  ( buffer :: BUFFER
  , fs :: FS
  , err :: EXCEPTION
  | eff
  )

getFile :: forall eff. Eff (ReadFileEffets eff) String
getFile = do
  buf <- readFile "src/p1/input.txt"
  toString UTF8 buf
