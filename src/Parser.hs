module Parser where

import Control.Applicative
import Data.Foldable (asum)
import Text.Read (readMaybe)

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String)
    }

instance Functor Parser where
    fmap f (Parser fa) = Parser fb
      where
        fb str
            | Just (a, str') <- fa str = Just (f a, str')
            | otherwise = Nothing

instance Applicative Parser where
    pure x = Parser (\str -> Just (x, str))
    liftA2 fab (Parser fa) (Parser fb) = Parser fc
      where
        fc str
            | Just (a, str') <- fa str
              , Just (b, str'') <- fb str' =
                Just (fab a b, str'')
            | otherwise = Nothing

instance Alternative Parser where
    empty = Parser (const Nothing)

    (<|>) (Parser f1) (Parser f2) = Parser f3
      where
        f3 str
            | Just r <- f1 str = Just r
            | otherwise = f2 str

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

parseUInt :: Parser Int
parseUInt = Parser f
  where
    parser = readMaybe <$> some (parseAnyChar ['0' .. '9'])
    f str
        | Just (Just i, str') <- runParser parser str = Just (i, str')
        | otherwise = Nothing

parseInt :: Parser Int
parseInt = Parser f
  where
    parser = readMaybe <$> some (parseAnyChar ('-' : ['0' .. '9']))
    f str
        | Just (Just i, str') <- runParser parser str = Just (i, str')
        | otherwise = Nothing

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = (,) <$> (parseOpen *> p <* parseComa) <*> (p <* parseClose)
  where
    parseOpen = parseChar '('
    parseClose = parseChar ')'
    parseComa = parseChar ','
