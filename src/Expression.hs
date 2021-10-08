module Expression where

import Control.Applicative ((<|>))
import Parser

data Operator = Plus | Minus | Product | Divide | Power
    deriving (Eq, Show)

data Expression = Num Double | Binary Expression Operator Expression
    deriving (Eq, Show)

parseExpression :: Parser Expression
parseExpression = withParenOrNot (parseBinary <|> parseNum) <* parseWhitespaces
  where
    parseNum = Num <$> parseDouble <* parseWhitespaces
    parseBinary =
        ( binaryWithPriority
            <$> parseNum
                <*> parseOperator'
                <*> parseExpression
        )
            <|> ( Binary <$> (parseOpen' *> parseExpression <* parseClose')
                    <*> parseOperator'
                    <*> parseExpression
                )
    binaryWithPriority l op r
        | (Binary l' op' r') <- r
          , priority op' < priority op =
            Binary (Binary l op l') op' r'
        | otherwise = Binary l op r
    parseOperator' = parseOperator <* parseWhitespaces
    parseOpen' = parseChar '(' *> parseWhitespaces
    parseClose' = parseChar ')' <* parseWhitespaces
    withParenOrNot parser =
        parser
            <|> parseOpen' *> withParenOrNot parser <* parseClose'

eval :: Expression -> Double
eval (Num n) = n
eval (Binary e1 op e2) = function op (eval e1) (eval e2)
  where
    function Plus = (+)
    function Minus = (-)
    function Product = (*)
    function Divide = (/)
    function Power = (**)

priority :: Operator -> Int
priority Plus = 1
priority Minus = 1
priority Product = 2
priority Divide = 2
priority Power = 3

parseOperator :: Parser Operator
parseOperator =
    Plus <$ parseChar '+'
        <|> Minus <$ parseChar '-'
        <|> Product <$ parseChar '*'
        <|> Divide <$ parseChar '/'
        <|> Power <$ parseChar '^'
