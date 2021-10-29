module Hal.SExpr where

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
    render' [v, SExpr s'] = render v <> " " <> render' s'
    render' [v1, v2] = render v1 <> " . " <> render v2
    render' (x : xs) = render x <> " " <> render' xs
    render' [] = ""

toPairs :: SExpr -> String
toPairs (Atom a) = a
toPairs (SExpr s) = "(" <> unwords (toPairs <$> s) <> ")"

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> SExpr <$> parseSExpr'

parseOpen :: Parser Char
parseOpen = parseChar '(' <* parseWhitespaces

parseClose :: Parser Char
parseClose = parseChar ')' <* parseWhitespaces

parseSExpr' :: Parser [SExpr]
parseSExpr' =
    ( parseOpen *> some parseSExpr <* parseClose
        -- syntaxic sugar
        <|> parseChar '\'' *> (unsugar <$> parseSExpr)
    )
        <* parseWhitespaces
  where
    unsugar a = [Atom "quote", a]

parseAtom :: Parser SExpr
parseAtom = Atom <$> (nil <|> ident) <* parseWhitespaces
  where
    ident = some (parsePred (`notElem` invalidChars))
    invalidChars = " \n()'" :: String
    nil = parseString "()"
