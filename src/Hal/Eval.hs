{-# LANGUAGE TupleSections #-}

module Hal.Eval where

import Control.Monad
import Data.Foldable
import Hal.SExpr
import Parser

type Env = [(String, SExprValue)]

class ToScheme a where
    toScheme :: a -> SExprValue

instance ToScheme Bool where
    toScheme True = Atom "#t"
    toScheme False = Atom "#f"

instance ToScheme Int where
    toScheme = Atom . show

data Error
    = WrongArgument String String Env
    | NotBounded String
    | NotImplemented SExpr
    | IntConvert SExprValue
    deriving (Eq, Show)

renderError :: Error -> String
renderError (WrongArgument cmd arg env) =
    unlines
        [ "Wrong argument for " <> cmd <> " " <> arg
        , "With env " <> show env
        ]
renderError (NotBounded a) = "Not bounded: " <> a
renderError (NotImplemented s) = "Not implemented: " <> toPairs s
renderError (IntConvert v) = "Int conversion error: " <> toPairsValue v

runEnv :: (Env -> Either Error a) -> Env -> Either Error (a, Env)
runEnv f e = (,e) <$> f e

eval :: SExpr -> Env -> Either Error (SExprValue, Env)
-- Builtins
eval (Atom "cons" : sx) e = runEnv (evalCons sx) e
eval (Atom "car" : sx) e = runEnv (evalCar sx) e
eval (Atom "cdr" : sx) e = runEnv (evalCdr sx) e
eval (Atom "eq?" : sx) e = runEnv (evalIsEq sx) e
eval (Atom "atom?" : sx) e = runEnv (evalIsAtom sx) e
eval (Atom "+" : sx) e = runEnv (evalArith (+) 0 sx) e
eval (Atom "-" : sx) e = runEnv (evalArith (-) 0 sx) e
eval (Atom "*" : sx) e = runEnv (evalArith (*) 1 sx) e
eval (Atom "div" : sx) e = runEnv (evalBin quot sx) e
eval (Atom "mod" : sx) e = runEnv (evalBin mod sx) e
eval (Atom "<" : sx) e = runEnv (evalBin (<) sx) e
-- Special forms
eval (Atom "quote" : sx) e = runEnv (const $ evalQuote sx) e
eval (Atom "define" : sx) e = evalDefine sx e
eval (Atom "let" : sx) e = evalLet sx e
eval (Atom "cond" : sx) e = evalCond sx e
eval ((SExpr (Atom "lambda" : sx)) : sx') e = runEnv (evalLambda sx sx') e
-- Bindings
eval l@(Atom "lambda" : _) e = Right (SExpr l, e)
-- Atom evaluation
eval [a@(Atom _)] e
    | Right _ <- toInt a = Right (a, e)
    | otherwise = runEnv (evalValue a) e
-- Define application
eval (a@(Atom _) : sx) e = do
    s <- evalValue a e
    eval (s : sx) e
-- Error
eval s _ = Left $ NotImplemented s

evalValue :: SExprValue -> Env -> Either Error SExprValue
evalValue a@(Atom "#f") _ = Right a
evalValue a@(Atom "#t") _ = Right a
evalValue a@(Atom n) e
    | Just v <- lookup n e = Right v
    | Right _ <- toInt a = Right a
    | otherwise = Left $ NotBounded n
evalValue (SExpr s) e = fst <$> eval s e

evalCons :: SExpr -> Env -> Either Error SExprValue
evalCons s@[_, _] e = SExpr <$> traverse (`evalValue` e) s
evalCons s e = Left $ wrongArgFor "cons" s e

evalCar :: SExpr -> Env -> Either Error SExprValue
evalCar [v] e
    | Right (SExpr (v' : _)) <- evalValue v e = Right v'
    | otherwise = Left $ wrongArgFor "car" v e
evalCar s e = Left $ wrongArgFor "car" s e

evalCdr :: SExpr -> Env -> Either Error SExprValue
evalCdr [v] e = evalCdr' =<< evalValue v e
  where
    evalCdr' (SExpr [_, s]) = Right s
    evalCdr' (SExpr (_ : vs)) = Right $ SExpr vs
    evalCdr' v' = Left $ wrongArgFor "cdr" v' e
evalCdr s e = Left $ wrongArgFor "cdr" s e

evalQuote :: SExpr -> Either Error SExprValue
evalQuote [SExpr s] = Right (SExpr (s <> [Atom "()"]))
evalQuote [v] = Right v
evalQuote s = Left $ wrongArgFor "quote" s []

evalIsAtom :: SExpr -> Env -> Either Error SExprValue
evalIsAtom [v] e = toScheme . isAtom <$> evalValue v e
  where
    isAtom (Atom _) = True
    isAtom _ = False
evalIsAtom s e = Left $ wrongArgFor "atom" s e

evalIsEq :: SExpr -> Env -> Either Error SExprValue
evalIsEq [v1, v2] e = do
    v1' <- evalValue v1 e
    v2' <- evalValue v2 e
    pure . toScheme $ eq v1' v2'
  where
    eq (Atom a1) (Atom a2) = a1 == a2
    eq _ _ = False
evalIsEq s e = Left $ wrongArgFor "eq?" s e

evalArith :: (Int -> Int -> Int) -> Int -> SExpr -> Env -> Either Error SExprValue
evalArith op d args e =
    compute =<< traverse (`evalToInt` e) args
  where
    compute [x] = Right . toScheme $ op d x
    compute (x : xs) = Right . toScheme $ foldl' op x xs
    compute _ = Left $ wrongArgFor "exp arithm" args e

evalToInt :: SExprValue -> Env -> Either Error Int
evalToInt v e = toInt =<< evalValue v e

evalBin :: ToScheme a => (Int -> Int -> a) -> SExpr -> Env -> Either Error SExprValue
evalBin op [v1, v2] e = do
    i1 <- evalToInt v1 e
    i2 <- evalToInt v2 e
    pure $ toScheme (op i1 i2)
evalBin _ s e = Left $ wrongArgFor "binary op" s e

evalLambda :: SExpr -> SExpr -> Env -> Either Error SExprValue
evalLambda [SExpr params, SExpr body] args e = do
    e' <- traverse define $ zip params args
    fst <$> eval body (e' <> e)
  where
    define (x, v) = evalDefine' [x, v] e
evalLambda _ s e = Left $ wrongArgFor "lambda" s e

evalDefine' :: SExpr -> Env -> Either Error (String, SExprValue)
evalDefine' [Atom a, s] e = (a,) <$> evalValue s e
evalDefine' [SExpr (Atom a : args), body] e =
    evalDefine' [Atom a, SExpr [Atom "lambda", SExpr args, body]] e
evalDefine' s e = Left $ wrongArgFor "define" s e

evalDefine :: SExpr -> Env -> Either Error (SExprValue, Env)
evalDefine s e = do
    e'@(_, v) <- evalDefine' s e
    pure (v, e' : e)

evalLet :: SExpr -> Env -> Either Error (SExprValue, Env)
evalLet [SExpr defines, s] e =
    runEnv (evalValue s) =<< getAllEnvs defines
  where
    getAllEnvs = foldMapM (envFromDefine <=< toSExpr)
    envFromDefine x = snd <$> evalDefine x e
    toSExpr (SExpr s') = Right s'
    toSExpr s' = Left $ wrongArgFor "let" s' e
    foldMapM a b = fold <$> traverse a b
evalLet s e = Left $ wrongArgFor "let" s e

evalCond :: SExpr -> Env -> Either Error (SExprValue, Env)
evalCond (SExpr [v1, v2] : xs) e = do
    v1' <- evalValue v1 e
    case v1' of
        Atom "#t" -> runEnv (evalValue v2) e
        Atom "#f" -> evalCond xs e
        s -> Left $ wrongArgFor "cond" s e
evalCond s e = Left $ wrongArgFor "cond" s e

toInt :: SExprValue -> Either Error Int
toInt s@(Atom a)
    | Just (i, _) <- runParser parseInt a = Right i
    | otherwise = Left $ IntConvert s
toInt s = Left $ IntConvert s

wrongArgFor :: Show a => String -> a -> Env -> Error
wrongArgFor name x e = WrongArgument name (show x) e
