module P1.Parer where

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
import Text.Parsing.Parser.String (char, string)
import Text.Parsing.Parser.Token (digit)

parse :: String -> Either ParseError (Array Instruction)
parse s = fromFoldable <$> runParser s parseInstructions

parseInstructions :: Parser String (List Instruction)
parseInstructions = sepBy (parseInstruction) (string ", ")

parseInstruction :: Parser String Instruction
parseInstruction = do parseLeft <|> parseRight
  where
    parseLeft :: Parser String Instruction
    parseLeft = do
      char 'L'
      L <$> parseInt

    parseRight :: Parser String Instruction
    parseRight = do
      char 'R'
      R <$> parseInt

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
