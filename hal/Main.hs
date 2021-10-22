import Data.Bifunctor
import Data.Maybe
import Hal.Eval (Env, eval, renderError)
import Hal.SExpr
import Parser
import System.Console.Haskeline
import System.Environment

import Control.Monad.IO.Class (liftIO)
import Data.Either
import qualified Hal.Eval as Eval

data Options = Options
    { opFile :: Maybe String
    , opInteractive :: Bool
    , opHelp :: Bool
    }

parseOptions :: [String] -> Options
parseOptions = foldr f def
  where
    def =
        Options
            { opFile = Nothing
            , opInteractive = False
            , opHelp = False
            }
    f "-i" o = o{opInteractive = True}
    f "-h" o = o{opHelp = True}
    f file o = o{opFile = Just file}

main :: IO ()
main = do
    options@Options{opFile = file, opInteractive = interactive} <-
        parseOptions <$> getArgs
    if valid options
        then do
            progName <- getProgName
            putStrLn $ progName <> ": [-i] [-h] [file]"
        else do
            runInputT defaultSettings $ do
                initialEnv file >>= toRepl interactive
  where
    valid Options{opFile = file, opInteractive = interactive, opHelp = help} =
        help || (not interactive && isNothing file)
    initialEnv Nothing = pure []
    initialEnv (Just file) = interpretFile file
    toRepl True = repl
    toRepl False = const $ pure ()

interpretFile :: String -> InputT IO Env
interpretFile file = do
    content <- liftIO $ readFile file
    parseEvalPrint content []

parseEvalPrint :: String -> Env -> InputT IO Env
parseEvalPrint input e = do
    let r = parseAndEval input e
    outputStrLn $ renderResult r
    pure $ fromRight e (snd <$> r)

repl :: Env -> InputT IO ()
repl e = evalLine =<< getInputLine "> "
  where
    evalLine Nothing = repl e
    evalLine (Just input) = parseEvalPrint input e >>= repl

data Error = ErParse String | ErEval Eval.Error
type Result = Either Error (SExprValue, Env)

renderResult :: Result -> String
renderResult (Left (ErEval err)) = renderError err
renderResult (Left (ErParse input)) = "Unable to parse: " <> input
renderResult (Right (v, e')) =
    unlines
        [ "r = " <> renderValue v
        , "e = " <> show (second toPairsValue <$> e')
        ]

parseAndEval :: String -> Env -> Either Error (SExprValue, Env)
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
