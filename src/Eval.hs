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

runEnv :: (Env -> SExprValue) -> Env -> (SExprValue, Env)
runEnv f e = (f e, e)

eval :: SExpr -> Env -> (SExprValue, Env)
-- Builtins
eval (Atom "cons" : sx) e = runEnv (evalCons sx) e
eval (Atom "car" : sx) e = runEnv (evalCar sx) e
eval (Atom "cdr" : sx) e = runEnv (evalCdr sx) e
eval (Atom "eq?" : sx) e = runEnv (evalIsEq sx) e
eval (Atom "atom?" : sx) e = runEnv (evalIsAtom sx) e
eval (Atom "+" : sx) e = runEnv (evalArith (+) 0 sx) e
eval (Atom "-" : sx) e = runEnv (evalArith (-) 0 sx) e
eval (Atom "*" : sx) e = runEnv (evalArith (*) 1 sx) e
eval (Atom "div" : sx) e = runEnv (evalBin div sx) e
eval (Atom "mod" : sx) e = runEnv (evalBin mod sx) e
eval (Atom "<" : sx) e = runEnv (evalBin (<) sx) e
-- Special forms
eval (Atom "quote" : sx) e = (evalQuote sx, e)
eval (Atom "define" : sx) e = evalDefine sx e
eval (Atom "let" : sx) e = evalLet sx e
eval (Atom "cond" : sx) e = evalCond sx e
eval ((SExpr (Atom "lambda" : sx)) : sx') e = runEnv (evalLambda sx sx') e
-- Bindings
eval l@(Atom "lambda" : _) e = (SExpr l, e)
eval [a@(Atom _)] e -- Atom is reduced to its maximum
    | Just _ <- toInt a = (a, e)
eval (a@(Atom _) : sx) e = eval (r : sx) e
  where
    r = evalValue a e
-- Error
eval s _ = error $ "Not implemented yet: " <> toPairs s

evalValue :: SExprValue -> Env -> SExprValue
evalValue a@(Atom "#f") _ = a
evalValue a@(Atom "#t") _ = a
evalValue a@(Atom n) e
    | Just v <- lookup n e = v
    | Just _ <- toInt a = a
    | otherwise = error $ "Not bounded: " <> n
evalValue (SExpr s) e = fst $ eval s e

evalCons :: SExpr -> Env -> SExprValue
evalCons s@[_, _] e = SExpr $ (`evalValue` e) <$> s
evalCons s e = wrongArgFor "cons" s e

evalCar :: SExpr -> Env -> SExprValue
evalCar [v] e
    | SExpr (v' : _) <- evalValue v e = v'
    | otherwise = wrongArgFor "car" v e
evalCar s e = wrongArgFor "car" s e

evalCdr :: SExpr -> Env -> SExprValue
evalCdr [v] e
    | SExpr [_, s] <- v' = s
    | SExpr (_ : vs) <- v' = SExpr vs
    | otherwise = wrongArgFor "cdr" v' e
  where
    v' = evalValue v e
evalCdr s e = wrongArgFor "cdr" s e

evalQuote :: SExpr -> SExprValue
evalQuote [v] = v
evalQuote s = wrongArgFor "quote" s []

evalIsAtom :: SExpr -> Env -> SExprValue
evalIsAtom [v] e
    | Atom _ <- v' = toScheme True
    | otherwise = toScheme False
  where
    v' = evalValue v e
evalIsAtom s e = wrongArgFor "atom" s e

evalIsEq :: SExpr -> Env -> SExprValue
evalIsEq [v1, v2] e = toScheme $ eq (evalValue v1 e) (evalValue v2 e)
  where
    eq (Atom a1) (Atom a2) = a1 == a2
    eq _ _ = False
evalIsEq s e = wrongArgFor "eq?" s e

evalArith :: (Int -> Int -> Int) -> Int -> SExpr -> Env -> SExprValue
evalArith op d args e = toScheme r
  where
    argsValues = (`evalValue` e) <$> args
    ints = traverse toInt argsValues
    r
        | Just [x] <- ints = op d x
        | Just (x : xs) <- ints = foldl' op x xs
        | otherwise = wrongArgFor "exp arithm" args e

evalBin :: ToScheme a => (Int -> Int -> a) -> SExpr -> Env -> SExprValue
evalBin op s@[v1, v2] e
    | Just i1 <- toInt v1', Just i2 <- toInt v2' = toScheme (op i1 i2)
    | otherwise = wrongArgFor "binary op" s e
  where
    v1' = evalValue v1 e
    v2' = evalValue v2 e
evalBin _ s e = wrongArgFor "binary op" s e

evalLambda :: SExpr -> SExpr -> Env -> SExprValue
evalLambda [SExpr params, SExpr body] args e = fst $ eval body (e' <> e)
  where
    e' = define <$> zip params args
    define (x, v) = evalDefine' [x, v] e
evalLambda _ s e = wrongArgFor "lambda" s e

evalDefine' :: SExpr -> Env -> (String, SExprValue)
evalDefine' [Atom a, s] e = (a, v)
  where
    v = evalValue s e
evalDefine' [SExpr (Atom a : args), body] e =
    evalDefine' [Atom a, SExpr [Atom "lambda", SExpr args, body]] e
evalDefine' s e = wrongArgFor "define" s e

evalDefine :: SExpr -> Env -> (SExprValue, Env)
evalDefine s e = (v, e' : e)
  where
    e'@(_, v) = evalDefine' s e

evalLet :: SExpr -> Env -> (SExprValue, Env)
evalLet [SExpr defines, s] e = (evalValue s e', e)
  where
    e' = foldMap (snd . (`evalDefine` e) . toSExpr) defines
    toSExpr (SExpr s') = s'
    toSExpr s' = wrongArgFor "let" s' e
evalLet s e = wrongArgFor "let" s e

evalCond :: SExpr -> Env -> (SExprValue, Env)
evalCond (SExpr [v1, v2] : xs) e =
    case evalValue v1 e of
        Atom "#t" -> (evalValue v2 e, e)
        Atom "#f" -> evalCond xs e
        s -> wrongArgFor "cond" s e
evalCond s e = wrongArgFor "cond" s e

toInt :: SExprValue -> Maybe Int
toInt (Atom a) = fst <$> runParser parseInt a
toInt _ = Nothing

wrongArgFor :: Show a => String -> a -> Env -> b
wrongArgFor name x e =
    error $
        unlines
            [ "wrong argument for: " <> name <> " " <> show x
            , "with env = " <> show e
            ]
