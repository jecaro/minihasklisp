module Eval where

import Data.Foldable
import Parser
import SExpr

type Env = [(String, SExprValue)]

class ToScheme a where
    toScheme :: a -> SExprValue

instance ToScheme Bool where
    toScheme True = Atom "#t"
    toScheme False = Atom "#f"

instance ToScheme Int where
    toScheme = Atom . show

eval :: Env -> SExpr -> (Env, SExprValue)
-- Builtins
eval e (Atom "cons" : sx) = (e, evalCons e sx)
eval e (Atom "car" : sx) = (e, evalCar e sx)
eval e (Atom "cdr" : sx) = (e, evalCar e sx)
eval e (Atom "eq?" : sx) = (e, evalIsEq e sx)
eval e (Atom "atom?" : sx) = (e, evalIsAtom e sx)
eval e (Atom "+" : sx) = (e, evalArith (+) 0 e sx)
eval e (Atom "-" : sx) = (e, evalArith (-) 0 e sx)
eval e (Atom "*" : sx) = (e, evalArith (*) 1 e sx)
eval e (Atom "div" : sx) = (e, evalBin div e sx)
eval e (Atom "mod" : sx) = (e, evalBin mod e sx)
eval e (Atom "<" : sx) = (e, evalBin (<) e sx)
-- Special forms
eval e (Atom "quote" : sx) = (e, evalQuote sx)
eval e (Atom "define" : sx) = evalDefine e sx
eval e (Atom "let" : sx) = evalLet e sx
eval e ((SExpr (Atom "lambda" : sx)) : sx') = (e, evalLambda e sx sx')
-- Bindings
eval e l@(Atom "lambda" : _) = (e, SExpr l)
eval e [a@(Atom _)] -- Atom is reduced to its maximum
    | Just _ <- toInt a = (e, a)
eval e (a@(Atom _) : sx) = eval e (r : sx)
  where
    r = evalValue e a
-- Error
eval _ s = error $ "Not implemented yet: " <> toPairs s

evalValue :: Env -> SExprValue -> SExprValue
evalValue e a@(Atom n)
    | Just v <- lookup n e = v
    | Just _ <- toInt a = a
    | otherwise = error "Not bounded"
evalValue e (SExpr s) = snd $ eval e s

evalCons :: Env -> SExpr -> SExprValue
evalCons e s@[_, _] = SExpr $ evalValue e <$> s
evalCons _ s = wrongArgument s

evalCar :: Env -> SExpr -> SExprValue
evalCar e [SExpr [Atom "cons", v1, _]] = evalValue e v1
evalCar _ s = wrongArgument s

evalCdr :: Env -> SExpr -> SExprValue
evalCdr e [SExpr [Atom "cons", _, v2]] = evalValue e v2
evalCdr _ s = wrongArgument s

evalQuote :: SExpr -> SExprValue
evalQuote [v] = v
evalQuote s = wrongArgument s

evalIsAtom :: Env -> SExpr -> SExprValue
evalIsAtom e [v]
    | Atom _ <- v' = toScheme True
    | SExpr s <- v', s == emptyList = toScheme True
    | otherwise = toScheme False
  where
    v' = evalValue e v
evalIsAtom _ s = wrongArgument s

evalIsEq :: Env -> SExpr -> SExprValue
evalIsEq e [v1, v2] = toScheme $ eq (evalValue e v1) (evalValue e v2)
  where
    eq (Atom a1) (Atom a2) = a1 == a2
    eq (SExpr s1) (SExpr s2) = s1 == emptyList && s2 == emptyList
    eq _ _ = False
evalIsEq _ s = wrongArgument s

evalArith :: (Int -> Int -> Int) -> Int -> Env -> SExpr -> SExprValue
evalArith op d e args = toScheme r
  where
    argsValues = evalValue e <$> args
    ints = traverse toInt argsValues
    r
        | Just [x] <- ints = op d x
        | Just (x : xs) <- ints = foldl' op x xs
        | otherwise = wrongArgument args

evalBin :: ToScheme a => (Int -> Int -> a) -> Env -> SExpr -> SExprValue
evalBin op e s@[v1, v2]
    | Just i1 <- toInt v1', Just i2 <- toInt v2' = toScheme (op i1 i2)
    | otherwise = wrongArgument s
  where
    v1' = evalValue e v1
    v2' = evalValue e v2
evalBin _ _ s = wrongArgument s

evalLambda :: Env -> SExpr -> SExpr -> SExprValue
evalLambda e [SExpr params, SExpr body] args = snd $ eval e' body
  where
    e' = foldMap define $ zip params args
    define (x, v) = fst $ evalDefine e [x, v]
evalLambda _ _ s = wrongArgument s

evalDefine :: Env -> SExpr -> (Env, SExprValue)
evalDefine e [Atom x, s] = ((x, v) : e, v)
  where
    v = evalValue e s
evalDefine _ s = wrongArgument s

evalLet :: Env -> SExpr -> (Env, SExprValue)
evalLet e [SExpr defines, s] = (e, evalValue e' s)
  where
    e' = foldMap (fst . evalDefine e . toSExpr) defines
    toSExpr (SExpr s') = s'
    toSExpr s' = wrongArgument s'
evalLet _ s = wrongArgument s

emptyList :: SExpr
emptyList = [Atom "()"]

toInt :: SExprValue -> Maybe Int
toInt (Atom a) = fst <$> runParser parseInt a
toInt _ = Nothing

wrongArgument :: Show a => a -> b
wrongArgument = error . ("wrong argument: " <>) . show
