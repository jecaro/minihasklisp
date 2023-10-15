module MiniHaskLisp.SExpr where

import Control.Applicative
import Parser

data SExpr = Atom String | SExpr [SExpr]
    deriving (Eq, Show)

render :: SExpr -> String
render (Atom a) = a
render (SExpr s) = "(" <> render' s <> ")"
  where
    render' [v] = render v
    render' [v, Atom "()"] = render v
    render' [v1, v2] = render v1 <> " . " <> render v2
    render' (x : xs) = render x <> " " <> render' xs
    render' [] = ""

toPairs :: SExpr -> String
toPairs (Atom a) = a
toPairs (SExpr s) = "(" <> unwords (toPairs <$> s) <> ")"

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> parseList <|> parsePair

parseDot :: Parser Char
parseDot = parseChar '.' <* parseWhitespaces

parsePair :: Parser SExpr
parsePair =
    flattenList
        <$> (parseOpen *> parseSExpr)
        <*> (parseDot *> parseSExpr <* parseClose)
  where
    flattenList a1 (SExpr s) = SExpr (a1 : s)
    flattenList a1 a2 = SExpr [a1, a2]

parseOpen :: Parser Char
parseOpen = parseChar '(' <* parseWhitespaces

parseClose :: Parser Char
parseClose = parseChar ')' <* parseWhitespaces

parseList :: Parser SExpr
parseList =
    SExpr
        <$> ( parseOpen *> (appendNil <$> some parseSExpr) <* parseClose
                <|> parseChar '\'' *> (unsugar <$> parseSExpr)
            )
  where
    appendNil s = s <> [Atom "()"]
    unsugar a = [Atom "quote", a, Atom "()"]

parseAtom :: Parser SExpr
parseAtom = Atom <$> (nil <|> ident) <* parseWhitespaces
  where
    ident = some (parsePred (`notElem` invalidChars))
    invalidChars = " \n()'." :: String
    nil = parseString "()"
