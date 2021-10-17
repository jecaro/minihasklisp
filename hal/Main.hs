import Control.Monad
import Data.Bifunctor
import Eval
import Parser
import SExpr
import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        ["-i"] -> runRepl []
        [file] -> void $ interpret file
        [file, "-i"] -> interpret file >>= runRepl
        _ -> putStrLn $ progName <> ": [-i|filename]"
  where
    runRepl = runInputT defaultSettings . repl

interpret :: String -> IO Env
interpret file = do
    (r, e) <- (`parseAndEval` []) <$> readFile file
    putStrLn $ unlines r
    return e

repl :: Env -> InputT IO ()
repl e = do
    mInput <- getInputLine "> "
    case mInput of
        Nothing -> repl e
        Just input -> do
            let (r, e') = parseAndEval input e
            outputStrLn $ unlines r
            repl e'

parseAndEval :: String -> Env -> ([String], Env)
parseAndEval content e =
    case runParser parseSExpr content of
        Nothing -> (["Unable to parse: ", content], e)
        Just (s, "") ->
            (
                [ "r = " <> renderValue v
                , "e = " <> show (second toPairsValue <$> e')
                ]
            , e'
            )
          where
            (v, e') = eval s e
        Just (s, x) -> parseAndEval x (snd $ eval s e)
