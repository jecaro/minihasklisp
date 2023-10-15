import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Maybe
import MiniHaskLisp.Eval
import MiniHaskLisp.ParseAndEval
import System.Console.Haskeline
import System.Environment

data Options = Options
    { opFile :: Maybe String
    , opEnvironment :: Bool
    , opInteractive :: Bool
    , opHelp :: Bool
    }

type PrintResult = Result -> InputT IO ()

main :: IO ()
main = do
    options@Options
        { opFile = file
        , opInteractive = interactive
        , opEnvironment = environment
        } <-
        parseOptions <$> getArgs
    if invalid options
        then do
            progName <- getProgName
            putStrLn $ progName <> ": [-i] [-h] [-e] [file]"
        else do
            runInputT defaultSettings $ do
                let printResult = outputStrLn . renderResult environment
                initialEnv printResult file >>= toRepl printResult interactive
  where
    invalid Options{opFile = file, opInteractive = interactive, opHelp = help} =
        help || (not interactive && isNothing file)
    initialEnv printResult (Just file) = interpretFile printResult file
    initialEnv _ Nothing = pure []
    toRepl printResult True = repl printResult
    toRepl _ False = const $ pure ()

parseOptions :: [String] -> Options
parseOptions = foldr f def
  where
    def =
        Options
            { opFile = Nothing
            , opEnvironment = False
            , opInteractive = False
            , opHelp = False
            }
    f "-i" o = o{opInteractive = True}
    f "-e" o = o{opEnvironment = True}
    f "-h" o = o{opHelp = True}
    f file o = o{opFile = Just file}

interpretFile :: PrintResult -> String -> InputT IO Env
interpretFile p file = do
    content <- liftIO $ readFile file
    parseEvalPrint p content []

parseEvalPrint :: PrintResult -> String -> Env -> InputT IO Env
parseEvalPrint printResult input e = do
    let r = parseAndEval input e
    printResult r
    pure $ fromRight e (snd <$> r)

repl :: PrintResult -> Env -> InputT IO ()
repl printResult e = evalLine =<< getInputLine "> "
  where
    evalLine Nothing = repl printResult e
    evalLine (Just input) =
        repl printResult =<< parseEvalPrint printResult input e
