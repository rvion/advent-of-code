module ParseUtils where

import Prelude
import Control.Alt ((<|>))
import Data.Array (fromFoldable, many)
import Data.Char.Unicode (digitToInt)
import Data.Either (Either)
import Data.Int (fromString)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import P1.Types (Instruction(..))
import Text.Parsing.Parser (ParseError, Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.String (char, satisfy, string)
import Text.Parsing.Parser.Token (digit)


lowerCaseLetter = satisfy
  (\c -> c >= 'a'
      && c <= 'z')


parseSingleDigitInt :: Parser String Int
parseSingleDigitInt = do
  d <- digitToInt <$> digit
  case d of
    Just num -> pure num
    Nothing -> fail "impossible"

parseInt :: Parser String Int
parseInt = do
  d <- fromCharArray >>> fromString <$> many digit
  case d of
    Just num -> pure num
    Nothing -> fail "impossible"
