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
    (e, r) <- parseAndEval [] <$> readFile file
    putStrLn $ unlines r
    return e

repl :: Env -> InputT IO ()
repl e = do
    mInput <- getInputLine "> "
    case mInput of
        Nothing -> repl e
        Just input -> do
            let (e', r) = parseAndEval e input
            outputStrLn $ unlines r
            repl e'

parseAndEval :: Env -> String -> (Env, [String])
parseAndEval e content =
    case runParser parseSExpr content of
        Nothing -> (e, ["Unable to parse: ", content])
        Just (s, "") ->
            ( e'
            ,
                [ "r = " <> toPairs' v
                , "e = " <> show (second toPairs' <$> e')
                ]
            )
          where
            (e', v) = eval e s
        Just (s, x) -> parseAndEval (fst $ eval e s) x
