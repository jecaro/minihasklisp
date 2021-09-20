module Parser where

import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import qualified Data.List as L
import Text.Read (readMaybe)

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String)
    }

instance Functor Parser where
    fmap f (Parser fa) =
        Parser
            ( \input -> do
                (x, input') <- fa input
                pure (f x, input')
            )

instance Applicative Parser where
    pure x = Parser (\input -> Just (x, input))
    liftA2 fab (Parser fa) (Parser fb) =
        Parser
            ( \input -> do
                (xa, input') <- fa input
                (xb, input'') <- fb input'
                pure (fab xa xb, input'')
            )

instance Alternative Parser where
    empty = Parser (const Nothing)

    (<|>) (Parser f1) (Parser f2) = Parser f3
      where
        f3 input
            | Just r <- f1 input = Just r
            | otherwise = f2 input

instance Monad Parser where
    (Parser fa) >>= fpb =
        Parser
            ( \input -> do
                (xa, input') <- fa input
                runParser (fpb xa) input'
            )

parseChar :: Char -> Parser Char
parseChar = Parser . parseCharF
  where
    parseCharF _ [] = Nothing
    parseCharF c (x : xs)
        | c == x = Just (x, xs)
        | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar = asum . fmap parseChar

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith = liftA2

parseString :: String -> Parser String
parseString str =
    Parser
        ( \input -> do
            rest <- L.stripPrefix str input
            pure (str, rest)
        )

parseUInt :: Parser Int
parseUInt = parseRead parser
  where
    parser = some (parseAnyChar ['0' .. '9'])

parseInt :: Parser Int
parseInt = parseRead parser
  where
    parser = some (parseAnyChar ('-' : ['0' .. '9']))

parseRead :: Read a => Parser String -> Parser a
parseRead p =
    Parser
        ( \input -> do
            (xStr, input') <- runParser p input
            x <- readMaybe xStr
            pure (x, input')
        )

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = (,) <$> (parseOpen *> p <* parseComa) <*> (p <* parseClose)
  where
    parseOpen = parseChar '('
    parseClose = parseChar ')'
    parseComa = parseChar ','

parseTuple' :: Parser a -> Parser (a, a)
parseTuple' p = do
    void $ parseChar '('
    a1 <- p
    void $ parseChar ','
    a2 <- p
    void $ parseChar ')'
    pure (a1, a2)
