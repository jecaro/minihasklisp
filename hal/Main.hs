import Data.Maybe
import Hal.Eval
import Hal.ParseAndEval
import System.Console.Haskeline
import System.Environment

import Control.Monad.IO.Class (liftIO)
import Data.Either

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
