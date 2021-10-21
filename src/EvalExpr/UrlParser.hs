module EvalExpr.UrlParser where

import Control.Applicative
import Parser

data Protocol = Http | Https
    deriving (Show)
type Domain = String
type Path = [String]

data Url = Url Protocol Domain Path
    deriving (Show)

parseProtocol :: Parser Protocol
parseProtocol = parseHttps <|> parseHttp
  where
    parseHttps = Https <$ parseString "https"
    parseHttp = Http <$ parseString "http"

parseSlash :: Parser Char
parseSlash = parseChar '/'

parseColon :: Parser Char
parseColon = parseChar ':'

parseAlpha :: Parser String
parseAlpha = some (parseAnyChar $ '.' : ['a' .. 'z'])

parseUrl :: Parser Url
parseUrl =
    Url <$> parseProtocol
        <* parseColon
        <* parseSlash
        <* parseSlash
        <*> parseAlpha
        <* parseSlash
        <*> many (parseAlpha <* optional parseSlash)
