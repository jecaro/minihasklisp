module JSONParser where

import Control.Applicative
import Parser

data JList = MkNum Int | MkList [JList] deriving (Show)

parseJList :: Parser JList
parseJList = parseMkNum <|> parseList
  where
    parseMkNum = MkNum <$> parseInt
    parseList =
        MkList
            <$> ( parseOpen *> parseSpace
                    *> many
                        ( parseJList <* parseSpace
                            <* optional (parseComa <* parseSpace)
                        )
                    <* parseSpace
                    <* parseClose
                )
    parseOpen = parseChar '['
    parseClose = parseChar ']'
    parseComa = parseChar ','
    parseSpace = many (parseChar ' ')
