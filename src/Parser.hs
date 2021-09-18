module Parser where

import Text.Read (readMaybe)

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String)
    }

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
parseSome = Parser . parseSomeF
  where
    parseSomeF p str
        | Just ((r1, r2), str') <- runParser (parseAnd p (parseMany p)) str =
            Just (r1 : r2, str')
        | otherwise = Nothing

parseUInt :: Parser Int
parseUInt = Parser parseUIntF
  where
    parseUIntF str
        | Just (intStr, str') <-
            runParser (parseSome (parseAnyChar ['0' .. '9'])) str =
            case readMaybe intStr of
                Nothing -> Nothing
                Just int -> Just (int, str')
        | otherwise = Nothing

parseInt :: Parser Int
parseInt = Parser parseIntF
  where
    parseIntF str
        | Just (intStr, str') <-
            runParser
                ( parseSome (parseAnyChar ('-' : ['0' .. '9']))
                )
                str =
            case readMaybe intStr of
                Nothing -> Nothing
                Just int -> Just (int, str')
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
