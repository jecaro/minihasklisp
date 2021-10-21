module Hal.SExpr where

import Control.Applicative
import Data.Functor
import Parser

data SExprValue = Atom String | SExpr SExpr
    deriving (Eq, Show)

type SExpr = [SExprValue]

render :: SExpr -> String
render s = "(" <> render' s <> ")"
  where
    render' [v] = renderValue v
    render' [v, Atom "()"] = renderValue v
    render' [v, SExpr s'] = renderValue v <> " " <> render' s'
    render' [v1, v2] = renderValue v1 <> " . " <> renderValue v2
    render' (x : xs) = renderValue x <> " " <> render' xs
    render' [] = ""

renderValue :: SExprValue -> String
renderValue (Atom a) = a
renderValue (SExpr s) = render s

toPairs :: SExpr -> String
toPairs x = "(" <> unwords (toPairsValue <$> x) <> ")"

toPairsValue :: SExprValue -> String
toPairsValue (Atom a) = a
toPairsValue (SExpr s) = toPairs s

parseSExpr :: Parser SExpr
parseSExpr = parseList <* parseWhitespaces

parseOpen :: Parser Char
parseOpen = parseChar '(' <* parseWhitespaces

parseClose :: Parser Char
parseClose = parseChar ')' <* parseWhitespaces

parseList :: Parser SExpr
parseList =
    parseOpen *> some (parseAtom <|> SExpr <$> parseSExpr) <* parseClose
        -- syntaxic sugar
        <|> parseString "'()" $> [Atom "quote", Atom "()"]
        <|> parseChar '\'' *> (unsugar <$> parseList)
  where
    unsugar s = [Atom "quote", SExpr (s <> [Atom "()"])]

parseAtom :: Parser SExprValue
parseAtom = Atom <$> (builtins <|> ident) <* parseWhitespaces
  where
    ident = some (parseAnyChar validChar)
    builtins =
        parseString "eq?"
            <|> parseString "atom?"
            <|> parseString "()"
            <|> parseString "+"
            <|> parseString "-"
            <|> parseString "*"
            <|> parseString "div"
            <|> parseString "mod"
            <|> parseString "<"
            <|> parseString "lambda"
            <|> parseString "let"
            <|> parseString "cond"
            <|> parseString "#f"
            <|> parseString "#t"
    validChar = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['?', '-']
