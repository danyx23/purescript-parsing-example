module Main where

import Prelude hiding (between,when)

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage )
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)
import Text.Parsing.Parser.String (char, eof, string)
import Text.Parsing.Parser.Combinators (choice, try)
import Record.Format (format)
import Data.Symbol (SProxy(..))
import Control.Lazy (defer)
import Data.List (List(..), many)


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
testValue = "left left up"

simpleMoveParser :: Parser String (List Move)
simpleMoveParser =
  (many $ choice
    [ do matchedString <- tokenParser.lexeme (string "up")
         pure MoveUp
    , (\ _ -> MoveDown) <$> tokenParser.lexeme (string "down")
    , (\ _ -> MoveLeft) <$> tokenParser.lexeme (string "left")
    , (\ _ -> MoveRight) <$> tokenParser.lexeme (string "right")
    ])
  <* eof

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
