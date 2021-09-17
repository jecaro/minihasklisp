module Parser where

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar c (x : xs)
    | c == x = Just (x, xs)
    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar (x : xs) str
    | Just r <- parseChar x str = Just r
    | otherwise = parseAnyChar xs str

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str
    | Just r <- p1 str = Just r
    | otherwise = p2 str

parseAnyChar' :: String -> Parser Char
parseAnyChar' chars = foldr parseOr (const Nothing) parsers
  where
    parsers = parseChar <$> chars

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 str
    | Just (r1, str1) <- p1 str = case p2 str1 of
        Nothing -> Nothing
        Just (r2, str2) -> Just ((r1, r2), str2)
    | otherwise = Nothing

parseMany :: Parser a -> Parser [a]
parseMany p str
    | (Just (r1, str1)) <- p str = case parseMany p str1 of
        Nothing -> Just ([r1], str1)
        Just (r2, str2) -> Just (r1 : r2, str2)
    | otherwise = Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p str
    | Just ((r1, r2), str') <- parseAnd p (parseMany p) str = Just (r1 : r2, str')
    | otherwise = Nothing
