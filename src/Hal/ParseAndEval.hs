module Hal.ParseAndEval where

import Data.Bifunctor (second)
import Hal.Eval hiding (Error)
import Hal.SExpr
import Parser

import qualified Hal.Eval as Eval

data Error = ErParse String | ErEval Eval.Error
    deriving (Eq, Show)
type Result = Either Error (SExpr, Env)

renderResult :: Result -> String
renderResult (Left (ErEval err)) = renderError err
renderResult (Left (ErParse input)) = "Unable to parse: " <> input
renderResult (Right (v, e')) =
    unlines
        [ "r = " <> render v
        , "e = " <> show (second toPairs <$> e')
        ]

parseAndEval :: String -> Env -> Either Error (SExpr, Env)
parseAndEval input e = do
    (s, input') <- withParseError $ runParser parseSExpr input
    r@(_, e') <- withEvalError $ eval s e
    case input' of
        "" -> Right r
        _ -> parseAndEval input' e'
  where
    withParseError Nothing = Left $ ErParse input
    withParseError (Just r) = Right r
    withEvalError (Left err) = Left $ ErEval err
    withEvalError (Right x) = Right x
