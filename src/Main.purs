module Main where

import Prelude hiding (between,when)

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage )
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser)
import Text.Parsing.Parser.String (char, eof)
import Text.Parsing.Parser.Combinators (choice, try)
import Record.Format (format)
import Data.Symbol (SProxy(..))
import Control.Lazy (defer)


tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

data SimpleArithmeticExpression
  = IntValue Int
  | Addition SimpleArithmeticExpression SimpleArithmeticExpression

instance showSimpleArithmetic :: Show SimpleArithmeticExpression where
  show (IntValue x) = show x
  show (Addition left right) = show left <> " + " <> show right

testValue :: String
testValue = "((100 + 23 ) + 5)"

intValueParser :: Parser String SimpleArithmeticExpression
intValueParser =
  IntValue <$> tokenParser.integer

additionParser :: Parser String SimpleArithmeticExpression
additionParser = do
    left <- defer (\ _ -> simpleArithmeticParser)
    op <- (char '+')
    right <- defer (\ _ -> simpleArithmeticParser)
    pure (Addition left right)

simpleArithmeticParser :: Parser String SimpleArithmeticExpression
simpleArithmeticParser =
  choice
    [ try $ tokenParser.parens $ defer (\ _ -> additionParser)
    , intValueParser
    ]
  <* eof



main :: Effect Unit
main = do
  let parseResult = runParser testValue simpleArithmeticParser
  case parseResult of
    Right parsed -> do
      log $ format
            (SProxy :: SProxy "Successfully parsed the expression: {expression}")
            {expression : parsed}
    Left err -> do
      log $ "Could not parse input - " <> (parseErrorMessage err)
