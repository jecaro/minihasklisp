module Eval where

import Data.Foldable
import Parser
import SExpr

type Env = [(String, SExpr)]

class ToScheme a where
    toScheme :: a -> SExprValue

instance ToScheme Bool where
    toScheme True = Atom "#t"
    toScheme False = Atom "#f"

instance ToScheme Int where
    toScheme = Atom . show

eval :: Env -> SExpr -> (Env, SExprValue)
-- Builtins
eval e (Atom "cons" : sx) = evalCons e sx
eval e (Atom "car" : sx) = evalCar e sx
eval e (Atom "cdr" : sx) = evalCar e sx
eval e (Atom "eq?" : sx) = evalEq e sx
eval e (Atom "atom?" : sx) = evalAtom e sx
eval e (Atom "+" : sx) = evalArith (+) 0 e sx
eval e (Atom "-" : sx) = evalArith (-) 0 e sx
eval e (Atom "*" : sx) = evalArith (*) 1 e sx
eval e (Atom "div" : sx) = evalBin div e sx
eval e (Atom "mod" : sx) = evalBin mod e sx
eval e (Atom "<" : sx) = evalBin (<) e sx
-- Special forms
eval e (Atom "quote" : sx) = evalQuote e sx
-- Error
eval _ s = error $ "Not implemented yet: " <> toPairs s

evalValue :: Env -> SExprValue -> (Env, SExprValue)
evalValue e a@(Atom _) = (e, a)
evalValue e (SExpr s) = eval e s

evalCons :: Env -> SExpr -> (Env, SExprValue)
evalCons e [s1, s2] = (e1 <> e2, SExpr [s1', s2'])
  where
    (e1, s1') = evalValue e s1
    (e2, s2') = evalValue e s2
evalCons _ _ = wrongArgument

evalCar :: Env -> SExpr -> (Env, SExprValue)
evalCar e [SExpr [Atom "cons", s1, _]] = evalValue e s1
evalCar _ _ = wrongArgument

evalCdr :: Env -> SExpr -> (Env, SExprValue)
evalCdr e [SExpr [Atom "cons", _, s2]] = evalValue e s2
evalCdr _ _ = wrongArgument

evalQuote :: Env -> SExpr -> (Env, SExprValue)
evalQuote e [s] = (e, s)
evalQuote _ _ = wrongArgument

evalAtom :: Env -> SExpr -> (Env, SExprValue)
evalAtom e [s]
    | Atom _ <- s' = (e', toScheme True)
    | SExpr s'' <- s', s'' == emptyList = (e', toScheme True)
    | otherwise = (e', toScheme False)
  where
    (e', s') = evalValue e s
evalAtom _ _ = wrongArgument

evalEq :: Env -> SExpr -> (Env, SExprValue)
evalEq e [s1, s2] = (e1 <> e2, toScheme $ eq' s1' s2')
  where
    eq' x1 x2
        | Atom a1 <- x1, Atom a2 <- x2 = a1 == a2
        | SExpr l1 <- x1, SExpr l2 <- x2 = l1 == emptyList && l2 == emptyList
        | otherwise = False
    (e1, s1') = evalValue e s1
    (e2, s2') = evalValue e s2
evalEq _ _ = wrongArgument

evalArith :: (Int -> Int -> Int) -> Int -> Env -> SExpr -> (Env, SExprValue)
evalArith op d e args = (concat es, toScheme r)
  where
    (es, args') = unzip (evalValue e <$> args)
    ints = sequenceA (toInt <$> args')
    r
        | Just [x] <- ints = op d x
        | Just (x : xs) <- ints = foldl' op x xs
        | otherwise = wrongArgument

evalBin :: ToScheme a => (Int -> Int -> a) -> Env -> SExpr -> (Env, SExprValue)
evalBin op e [s1, s2]
    | Just i1 <- toInt s1', Just i2 <- toInt s2' = (e1 <> e2, toScheme (op i1 i2))
    | otherwise = wrongArgument
  where
    (e1, s1') = evalValue e s1
    (e2, s2') = evalValue e s2
evalBin _ _ _ = wrongArgument

emptyList :: SExpr
emptyList = [Atom "()"]

toInt :: SExprValue -> Maybe Int
toInt (Atom a) = fst <$> runParser parseInt a
toInt _ = Nothing

wrongArgument :: a
wrongArgument = error "wrong argument"
