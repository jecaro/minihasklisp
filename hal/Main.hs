import Control.Monad
import Data.Bifunctor
import Eval
import Parser
import SExpr
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        ["-i"] -> repl []
        [file] -> void $ interpret file
        [file, "-i"] -> interpret file >>= repl
        _ -> putStrLn $ progName <> ": [-i|filename]"

interpret :: String -> IO Env
interpret file = do
    (e, r) <- parseAndEval [] <$> readFile file
    putStrLn $ unlines r
    return e

repl :: Env -> IO ()
repl e = do
    (e', r) <- parseAndEval e <$> getLine
    putStrLn $ unlines r
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
