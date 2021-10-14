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
eval e (Atom "cdr" : sx) = (e, evalCdr e sx)
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
eval e (Atom "cond" : sx) = evalCond e sx
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
evalValue _ a@(Atom "#f") = a
evalValue _ a@(Atom "#t") = a
evalValue e a@(Atom n)
    | Just v <- lookup n e = v
    | Just _ <- toInt a = a
    | otherwise = error $ "Not bounded: " <> n
evalValue e (SExpr s) = snd $ eval e s

evalCons :: Env -> SExpr -> SExprValue
evalCons e s@[_, _] = SExpr $ evalValue e <$> s
evalCons e s = wrongArgFor "cons" e s

evalCar :: Env -> SExpr -> SExprValue
evalCar e [v]
    | SExpr (v' : _) <- evalValue e v = v'
    | otherwise = wrongArgFor "car" e v
evalCar e s = wrongArgFor "car" e s

evalCdr :: Env -> SExpr -> SExprValue
evalCdr e [v]
    | SExpr [_, s] <- evalValue e v = s
    | SExpr (_ : vs) <- evalValue e v = SExpr vs
    | otherwise = wrongArgFor "cdr" e $ evalValue e v
evalCdr e s = wrongArgFor "cdr" e s

evalQuote :: SExpr -> SExprValue
evalQuote [v] = v
evalQuote s = wrongArgFor "quote" [] s

evalIsAtom :: Env -> SExpr -> SExprValue
evalIsAtom e [v]
    | Atom _ <- v' = toScheme True
    | otherwise = toScheme False
  where
    v' = evalValue e v
evalIsAtom e s = wrongArgFor "atom" e s

evalIsEq :: Env -> SExpr -> SExprValue
evalIsEq e [v1, v2] = toScheme $ eq (evalValue e v1) (evalValue e v2)
  where
    eq (Atom a1) (Atom a2) = a1 == a2
    eq _ _ = False
evalIsEq e s = wrongArgFor "eq?" e s

evalArith :: (Int -> Int -> Int) -> Int -> Env -> SExpr -> SExprValue
evalArith op d e args = toScheme r
  where
    argsValues = evalValue e <$> args
    ints = traverse toInt argsValues
    r
        | Just [x] <- ints = op d x
        | Just (x : xs) <- ints = foldl' op x xs
        | otherwise = wrongArgFor "exp arithm" e args

evalBin :: ToScheme a => (Int -> Int -> a) -> Env -> SExpr -> SExprValue
evalBin op e s@[v1, v2]
    | Just i1 <- toInt v1', Just i2 <- toInt v2' = toScheme (op i1 i2)
    | otherwise = wrongArgFor "binary op" e s
  where
    v1' = evalValue e v1
    v2' = evalValue e v2
evalBin _ e s = wrongArgFor "binary op" e s

evalLambda :: Env -> SExpr -> SExpr -> SExprValue
evalLambda e [SExpr params, SExpr body] args = snd $ eval (e' <> e) body
  where
    e' = define <$> zip params args
    define (x, v) = evalDefine' e [x, v]
evalLambda e _ s = wrongArgFor "lambda" e s

evalDefine' :: Env -> SExpr -> (String, SExprValue)
evalDefine' e [Atom a, s] = (a, v)
  where
    v = evalValue e s
evalDefine' e [SExpr (Atom a : args), body] =
    evalDefine' e [Atom a, SExpr [Atom "lambda", SExpr args, body]]
evalDefine' e s = wrongArgFor "define" e s

evalDefine :: Env -> SExpr -> (Env, SExprValue)
evalDefine e s = (e' : e, v)
  where
    e'@(_, v) = evalDefine' e s

evalLet :: Env -> SExpr -> (Env, SExprValue)
evalLet e [SExpr defines, s] = (e, evalValue e' s)
  where
    e' = foldMap (fst . evalDefine e . toSExpr) defines
    toSExpr (SExpr s') = s'
    toSExpr s' = wrongArgFor "let" e s'
evalLet e s = wrongArgFor "let" e s

evalCond :: Env -> SExpr -> (Env, SExprValue)
evalCond e (SExpr [v1, v2] : xs) =
    case evalValue e v1 of
        Atom "#t" -> (e, evalValue e v2)
        Atom "#f" -> evalCond e xs
        s -> wrongArgFor "cond" e s
evalCond e s = wrongArgFor "cond" e s

toInt :: SExprValue -> Maybe Int
toInt (Atom a) = fst <$> runParser parseInt a
toInt _ = Nothing

wrongArgFor :: Show a => String -> Env -> a -> b
wrongArgFor name e x =
    error $
        unlines
            [ "wrong argument for: " <> name <> " " <> show x
            , "with env = " <> show e
            ]
