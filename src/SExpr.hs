module SExpr where

import Control.Applicative
import Parser

data SExprValue = Atom String | SExpr SExpr
    deriving (Eq, Show)

type SExpr = [SExprValue]

render :: SExpr -> String
render se = "(" <> unwords (render' <$> se) <> ")"
  where
    render' :: SExprValue -> String
    render' (Atom a) = a
    render' (SExpr se') = render se'

toPairs :: SExpr -> String
toPairs [x] = toPairs' x
toPairs [x1, x2] = "(" <> toPairs' x1 <> " . " <> toPairs' x2 <> ")"
toPairs x = "(" <> unwords (toPairs' <$> x) <> ")"

toPairs' :: SExprValue -> String
toPairs' (Atom a) = a
toPairs' (SExpr s) = toPairs s

parseSExpr :: Parser SExpr
parseSExpr = parseList <* parseWhitespaces

parseOpen :: Parser Char
parseOpen = parseChar '(' <* parseWhitespaces

parseClose :: Parser Char
parseClose = parseChar ')' <* parseWhitespaces

parseList :: Parser SExpr
parseList = parseOpen *> some (parseAtom <|> SExpr <$> parseSExpr) <* parseClose

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
    validChar = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
