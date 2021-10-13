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
evalCons _ _ = wrongArgument

evalCar :: Env -> SExpr -> SExprValue
evalCar e [SExpr [Atom "cons", s1, _]] = evalValue e s1
evalCar _ _ = wrongArgument

evalCdr :: Env -> SExpr -> SExprValue
evalCdr e [SExpr [Atom "cons", _, s2]] = evalValue e s2
evalCdr _ _ = wrongArgument

evalQuote :: SExpr -> SExprValue
evalQuote [s] = s
evalQuote _ = wrongArgument

evalIsAtom :: Env -> SExpr -> SExprValue
evalIsAtom e [s]
    | Atom _ <- v = toScheme True
    | SExpr s' <- v, s' == emptyList = toScheme True
    | otherwise = toScheme False
  where
    v = evalValue e s
evalIsAtom _ _ = wrongArgument

evalIsEq :: Env -> SExpr -> SExprValue
evalIsEq e [s1, s2] = toScheme $ eq (evalValue e s1) (evalValue e s2)
  where
    eq v1 v2
        | Atom a1 <- v1, Atom a2 <- v2 = a1 == a2
        | SExpr l1 <- v1, SExpr l2 <- v2 = l1 == emptyList && l2 == emptyList
        | otherwise = False
evalIsEq _ _ = wrongArgument

evalArith :: (Int -> Int -> Int) -> Int -> Env -> SExpr -> SExprValue
evalArith op d e args = toScheme r
  where
    argsValues = evalValue e <$> args
    ints = traverse toInt argsValues
    r
        | Just [x] <- ints = op d x
        | Just (x : xs) <- ints = foldl' op x xs
        | otherwise = wrongArgument

evalBin :: ToScheme a => (Int -> Int -> a) -> Env -> SExpr -> SExprValue
evalBin op e [s1, s2]
    | Just i1 <- toInt v1, Just i2 <- toInt v2 = toScheme (op i1 i2)
    | otherwise = wrongArgument
  where
    v1 = evalValue e s1
    v2 = evalValue e s2
evalBin _ _ _ = wrongArgument

evalLambda :: Env -> SExpr -> SExpr -> SExprValue
evalLambda e [SExpr params, SExpr body] args = snd $ eval (e' <> e) body
  where
    e' = foldMap define $ zip params args
    define (x, v) = fst $ evalDefine e [x, v]
evalLambda _ _ _ = wrongArgument

evalDefine :: Env -> SExpr -> (Env, SExprValue)
evalDefine e [Atom x, s] = ((x, v) : e, v)
  where
    v = evalValue e s
evalDefine _ _ = wrongArgument

emptyList :: SExpr
emptyList = [Atom "()"]

toInt :: SExprValue -> Maybe Int
toInt (Atom a) = fst <$> runParser parseInt a
toInt _ = Nothing

wrongArgument :: a
wrongArgument = error "wrong argument"
