module SExpr where

import Control.Applicative
import Parser

data Atom = Nil | Atom String
    deriving (Eq, Show)

data SExpr = SAtom Atom | Pair SExpr SExpr
    deriving (Eq, Show)

parseSExpr :: Parser SExpr
parseSExpr = parseChar '\'' *> parseSExprWithoutQuote

render :: SExpr -> String
render (Pair s1 s2@(Pair _ _)) = "(" <> render s1 <> renderList s2 <> ")"
  where
    renderList (SAtom Nil) = ""
    renderList a@(SAtom _) = " " <> render a
    renderList (Pair s ss) = " " <> render s <> renderList ss
render (Pair s1 s2@(SAtom _)) = "(" <> render s1 <> " . " <> render s2 <> ")"
render (SAtom Nil) = "()"
render (SAtom (Atom a)) = a

toPairs :: SExpr -> String
toPairs (Pair s1 s2) = "(" <> toPairs s1 <> " . " <> toPairs s2 <> ")"
toPairs (SAtom Nil) = "()"
toPairs (SAtom (Atom a)) = a

parseSExprWithoutQuote :: Parser SExpr
parseSExprWithoutQuote = (parseAtom <|> parsePair <|> parseList) <* parseWhitespaces

parseOpen :: Parser Char
parseOpen = parseChar '(' <* parseWhitespaces

parseClose :: Parser Char
parseClose = parseChar ')' <* parseWhitespaces

parseDot :: Parser Char
parseDot = parseChar '.' <* parseWhitespaces

parseList :: Parser SExpr
parseList = parseOpen *> parseListWithoutParen <* parseClose

parseListWithoutParen :: Parser SExpr
parseListWithoutParen =
    Pair
        <$> parseSExprWithoutQuote
            <*> (parseListWithoutParen <|> pure (SAtom Nil))

parsePair :: Parser SExpr
parsePair =
    Pair
        <$> (parseOpen *> parseSExprWithoutQuote <* parseDot)
            <*> parseSExprWithoutQuote <* parseClose

parseAtom :: Parser SExpr
parseAtom =
    SAtom Nil <$ parseString "()"
        <|> SAtom . Atom <$> some (parseAnyChar alphaNum)
  where
    alphaNum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
