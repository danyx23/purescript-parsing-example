module Main where

import Prelude hiding (between,when)

import Control.Lazy (defer)
import Data.Either (Either(..))
import Data.List (List(..), many)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Record.Format (format)
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.Combinators (choice, try)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, eof, string, whiteSpace)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)


tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

type Position = { x :: Int, y :: Int}

instance showMove :: Show Move where
  show MoveUp = "up"
  show MoveDown = "down"
  show MoveLeft = "left"
  show MoveRight = "right"

testValue :: String
testValue = " left left up"

moveLiteralParser :: String -> Move -> Parser String Move
moveLiteralParser literal moveValue = do
  matchedString <- tokenParser.lexeme (string literal)
  pure moveValue

singleMoveParser :: Parser String Move
singleMoveParser =
  choice
    [ moveLiteralParser "up" MoveUp
    , moveLiteralParser "down" MoveDown
    , moveLiteralParser "left" MoveLeft
    , moveLiteralParser "right" MoveRight
    ]

simpleMoveParser :: Parser String (List Move)
simpleMoveParser = do
  _ <- whiteSpace
  moves <- many singleMoveParser
  _ <- eof
  pure moves

interpretMoves :: List Move -> Position -> Position
interpretMoves Nil pos = pos
interpretMoves (Cons MoveUp tail)    pos = interpretMoves tail {x: pos.x,   y: pos.y-1}
interpretMoves (Cons MoveDown tail)  pos = interpretMoves tail {x: pos.x,   y: pos.y+1}
interpretMoves (Cons MoveLeft tail)  pos = interpretMoves tail {x: pos.x-1, y: pos.y}
interpretMoves (Cons MoveRight tail) pos = interpretMoves tail {x: pos.x+1, y: pos.y}

main :: Effect Unit
main = do
  let parseResult = runParser testValue simpleMoveParser
  case parseResult of
    Right parsed -> do
      let endPosition = interpretMoves parsed {x: 0, y: 0}
      log $ format
            (SProxy :: SProxy "Successfully parsed the expression: {expression} - the final position is: {position}")
            {expression : parsed, position : endPosition}
    Left err -> do
      log $ "Could not parse input - " <> (parseErrorMessage err)
