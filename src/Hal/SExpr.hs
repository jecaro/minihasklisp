module Hal.SExpr where

import Control.Applicative
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
        <|> parseChar '\''
            *> ( unsugar
                    <$> ( parseAtom
                            <|> (SExpr <$> parseList)
                        )
               )
  where
    unsugar a = [Atom "quote", a]

parseAtom :: Parser SExprValue
parseAtom = Atom <$> (nil <|> ident) <* parseWhitespaces
  where
    ident = some (parsePred (`notElem` invalidChars))
    invalidChars = " \n()'" :: String
    nil = parseString "()"
