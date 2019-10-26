module Main where

import Prelude hiding (between,when)

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)

simpleArithmeticTokenParser :: TokenParser
simpleArithmeticTokenParser = makeTokenParser emptyDef

testValue :: String
testValue = "100"

simpleArithmeticParser :: Parser String Int
simpleArithmeticParser = simpleArithmeticTokenParser.integer

main :: Effect Unit
main = do
  let parseResult = runParser testValue simpleArithmeticParser
  case parseResult of
    Right parsed -> do
      log "Parsed a number!"
    Left err -> do
      log "Could not parse..."
