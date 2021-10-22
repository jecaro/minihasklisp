import Data.Bifunctor
import Data.Maybe
import Hal.Eval
import Hal.SExpr
import Parser
import System.Console.Haskeline
import System.Environment

data Options = Options
    { opFile :: Maybe String
    , opInteractive :: Bool
    , opHelp :: Bool
    }

parseOptions :: [String] -> Options
parseOptions = foldr f def
  where
    def = Options{opFile = Nothing, opInteractive = False, opHelp = False}
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
        else initialEnv file >>= toRepl interactive
  where
    valid Options{opFile = file, opInteractive = interactive, opHelp = help} =
        help || (not interactive && isNothing file)
    initialEnv Nothing = pure []
    initialEnv (Just file) = interpret file
    toRepl True = runInputT defaultSettings . repl
    toRepl False = const mempty

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
        Just (s, x) ->
            case eval s e of
                Left err -> ([renderError err], e)
                Right r@(_, e') -> case x of
                    "" -> (renderResult r, e)
                    x' -> parseAndEval x' e'
  where
    renderError (WrongArgument cmd arg env) =
        unlines
            [ "Wrong argument for " <> cmd <> " " <> arg
            , "With env " <> show env
            ]
    renderError (NotBounded a) = "Not bounded: " <> a
    renderError (NotImplemented s) = "Not implemented: " <> toPairs s
    renderError (IntConvert v) = "Int conversion error: " <> toPairsValue v
    renderResult (v, e') =
        [ "r = " <> renderValue v
        , "e = " <> show (second toPairsValue <$> e')
        ]
