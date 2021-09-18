module Parser where

import Control.Applicative
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

parseChar :: Char -> Parser Char
parseChar = Parser . parseCharF
  where
    parseCharF _ [] = Nothing
    parseCharF c (x : xs)
        | c == x = Just (x, xs)
        | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar = Parser . parseAnyCharF
  where
    parseAnyCharF [] _ = Nothing
    parseAnyCharF (x : xs) str
        | Just r <- runParser (parseChar x) str = Just r
        | otherwise = parseAnyCharF xs str

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser f1) (Parser f2) = Parser parseOrF
  where
    parseOrF str
        | Just r <- f1 str = Just r
        | otherwise = f2 str

parseAnyChar' :: String -> Parser Char
parseAnyChar' chars = foldr parseOr (Parser $ const Nothing) parsers
  where
    parsers = parseChar <$> chars

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser f1) (Parser f2) = Parser parseAndF
  where
    parseAndF str
        | Just (r1, str1) <- f1 str = case f2 str1 of
            Nothing -> Nothing
            Just (r2, str2) -> Just ((r1, r2), str2)
        | otherwise = Nothing

parseMany :: Parser a -> Parser [a]
parseMany = Parser . parseManyF . runParser
  where
    parseManyF f str
        | (Just (r1, str1)) <- f str = case parseManyF f str1 of
            Nothing -> Just ([r1], str1)
            Just (r2, str2) -> Just (r1 : r2, str2)
        | otherwise = Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseUInt :: Parser Int
parseUInt = Parser f
  where
    parser = readMaybe <$> parseSome (parseAnyChar ['0' .. '9'])
    f str
        | Just (Just i, str') <- runParser parser str = Just (i, str')
        | otherwise = Nothing

parseInt :: Parser Int
parseInt = Parser f
  where
    parser = readMaybe <$> parseSome (parseAnyChar ('-' : ['0' .. '9']))
    f str
        | Just (Just i, str') <- runParser parser str = Just (i, str')
        | otherwise = Nothing

parseTuple :: Parser a -> Parser (a, a)
parseTuple = Parser . parseTupleF
  where
    parseTupleF p str
        | Just (((((_, a1), _), a2), _), str') <-
            runParser
                ( parseOpen
                    `parseAnd` p
                    `parseAnd` parseComa
                    `parseAnd` p
                    `parseAnd` parseClose
                )
                str =
            Just ((a1, a2), str')
        | otherwise = Nothing

    parseOpen = parseChar '('
    parseClose = parseChar ')'
    parseComa = parseChar ','
