module MiniHaskLisp.ParseAndEval where

import Data.Bifunctor (second)
import MiniHaskLisp.Eval hiding (Error)
import MiniHaskLisp.SExpr
import Parser

import qualified MiniHaskLisp.Eval as Eval

data Error = ErParse String | ErEval Eval.Error
    deriving (Eq, Show)
type Result = Either Error (SExpr, Env)

renderResult :: Bool -> Result -> String
renderResult _ (Left (ErEval err)) = renderError err
renderResult _ (Left (ErParse input)) = "Unable to parse: " <> input
renderResult True (Right (v, e')) =
    unlines
        [ "r = " <> render v
        , "e = " <> show (second toPairs <$> e')
        ]
renderResult False (Right (v, _)) = render v

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
