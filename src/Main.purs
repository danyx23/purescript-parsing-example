module Main where

import Prelude hiding (between,when)

import Data.Either (Either(..))
import Data.List (List(..), many, concat)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Record.Format (format)
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (eof, string, whiteSpace)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)
import Data.Unfoldable (replicate)
import Control.Lazy (fix)

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

data Expression
  = SingleMove Move
  | Repeat Int (List Expression)

type Position = { x :: Int, y :: Int}

instance showMove :: Show Move where
  show MoveUp = "up"
  show MoveDown = "down"
  show MoveLeft = "left"
  show MoveRight = "right"

instance showExpression :: Show Expression where
  show (SingleMove move) = show move
  show (Repeat count moves) =
    format
      (SProxy :: SProxy "repeat {count} [{moves}]")
      {count : count, moves : moves}

testValue :: String
testValue = """
up repeat 2 [ left repeat 2 [ up ] ]
"""

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

repeatParser :: Parser String Expression
repeatParser = do
  _ <- tokenParser.lexeme (string "repeat")
  repeatCount <- tokenParser.integer
  moves <- tokenParser.brackets (many moveExpressionParser)
  pure (Repeat repeatCount moves)

moveExpressionParser :: Parser String Expression
moveExpressionParser =
    fix \_ -> -- use fix to make this parser lazy so that the recursion is accepted by the compiler
        choice
        [ repeatParser
        , map SingleMove singleMoveParser ]

moveProgramParser :: Parser String (List Expression)
moveProgramParser = do
  _ <- whiteSpace
  moves <- many moveExpressionParser
  _ <- eof
  pure moves

simplifyExpressionToMoves :: Expression -> List Move
simplifyExpressionToMoves (SingleMove move) = Cons move Nil
simplifyExpressionToMoves (Repeat count nested) =
    bind nested simplifyExpressionToMoves
    # replicate count
    # concat


interpretMoves :: List Move -> Position -> Position
interpretMoves Nil pos = pos
interpretMoves (Cons (MoveUp) tail)    pos = interpretMoves tail {x: pos.x,   y: pos.y-1}
interpretMoves (Cons (MoveDown) tail)  pos = interpretMoves tail {x: pos.x,   y: pos.y+1}
interpretMoves (Cons (MoveLeft) tail)  pos = interpretMoves tail {x: pos.x-1, y: pos.y}
interpretMoves (Cons (MoveRight) tail) pos = interpretMoves tail {x: pos.x+1, y: pos.y}



main :: Effect Unit
main = do
  let parseResult = runParser testValue moveProgramParser
  case parseResult of
    Right parsed -> do
        let moves = bind parsed simplifyExpressionToMoves
        let endPosition = interpretMoves moves {x: 0, y: 0}
        let message = format
                        (SProxy :: SProxy "Successfully parsed the expression: {expression} - the final position is: {position}")
                        {expression : parsed, position : endPosition}
        log message
    Left err -> do
      log $ "Could not parse input - " <> (parseErrorMessage err)
