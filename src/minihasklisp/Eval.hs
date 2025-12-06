module Eval (Error (..), Env, eval, renderError) where

import Control.Monad (liftM2, (<=<))
import Parser (Parser (..), parseInt)
import SExpr (SExpr (..), toPairs)

type Env = [(String, SExpr)]

class ToScheme a where
    toScheme :: a -> SExpr

instance ToScheme Bool where
    toScheme True = Atom "#t"
    toScheme False = Atom "#f"

instance ToScheme Int where
    toScheme = Atom . show

data Error
    = WrongArgument String String Env
    | NotBound String
    | NotImplemented SExpr
    | IntConvert SExpr
    | NotAList SExpr
    deriving (Eq, Show)

renderError :: Error -> String
renderError (WrongArgument cmd arg env) =
    unlines
        [ "Wrong argument for " <> cmd <> " " <> arg
        , "With env " <> show env
        ]
renderError (NotBound a) = "Not bound: " <> a
renderError (NotImplemented s) = "Not implemented: " <> toPairs s
renderError (IntConvert v) = "Int conversion error: " <> toPairs v
renderError (NotAList s) = "Not a proper list: " <> toPairs s

runEnv :: (Env -> Either Error a) -> Env -> Either Error (a, Env)
runEnv f e = (,e) <$> f e

eval :: SExpr -> Env -> Either Error (SExpr, Env)
-- Builtins
eval (SExpr (Atom "cons" : sx)) e = runEnv (evalCons sx) e
eval (SExpr (Atom "car" : sx)) e = runEnv (evalCar sx) e
eval (SExpr (Atom "cdr" : sx)) e = runEnv (evalCdr sx) e
eval (SExpr (Atom "eq?" : sx)) e = runEnv (evalIsEq sx) e
eval (SExpr (Atom "atom?" : sx)) e = runEnv (evalIsAtom sx) e
eval (SExpr (Atom "+" : sx)) e = runEnv (evalArith (+) 0 sx) e
eval (SExpr (Atom "-" : sx)) e = runEnv (evalArith (-) 0 sx) e
eval (SExpr (Atom "*" : sx)) e = runEnv (evalArith (*) 1 sx) e
eval (SExpr (Atom "div" : sx)) e = runEnv (evalBin quot sx) e
eval (SExpr (Atom "mod" : sx)) e = runEnv (evalBin mod sx) e
eval (SExpr (Atom "<" : sx)) e = runEnv (evalBin (<) sx) e
-- Special forms
eval (SExpr (Atom "quote" : sx)) e = runEnv (const $ evalQuote sx) e
eval (SExpr (Atom "define" : sx)) e = evalDefine sx e
eval (SExpr (Atom "let" : sx)) e = evalLet sx e
eval (SExpr (Atom "cond" : sx)) e = evalCond sx e
eval (SExpr ((SExpr (Atom "lambda" : sx)) : sx')) e = runEnv (evalLambda sx sx') e
-- Lambda nothing to evaluate
eval l@(SExpr (Atom "lambda" : _)) e = Right (l, e)
-- Function application
eval (SExpr ((Atom n) : sx)) e
    | Just v <- lookup n e = eval (SExpr (v : sx)) e
    | otherwise = Left $ NotBound n
-- Atom evaluation
eval a@(Atom n) e
    -- Builtin types
    | Right _ <- toInt a = Right (a, e)
    | n == "#f" = Right (a, e)
    | n == "#t" = Right (a, e)
    -- Bindings
    | Just v <- lookup n e = Right (v, e)
    | otherwise = Left $ NotBound n
-- Error
eval s _ = Left $ NotImplemented s

-- Evaluate discarding the new env
eval' :: SExpr -> Env -> Either Error SExpr
eval' v e = fst <$> eval v e

evalCons :: [SExpr] -> Env -> Either Error SExpr
evalCons [v1, v2, Atom "()"] e = do
    v1' <- eval' v1 e
    s2 <- toSExprList <$> eval' v2 e
    pure $ SExpr (v1' : s2)
  where
    toSExprList a@(Atom _) = [a]
    toSExprList (SExpr s) = s
evalCons s e = Left $ wrongArgFor "cons" s e

evalCar :: [SExpr] -> Env -> Either Error SExpr
evalCar [v, Atom "()"] e
    | Right (SExpr (v' : _)) <- eval' v e = Right v'
    | otherwise = Left $ wrongArgFor "car" v e
evalCar s e = Left $ wrongArgFor "car" s e

evalCdr :: [SExpr] -> Env -> Either Error SExpr
evalCdr [v, Atom "()"] e = evalCdr' =<< eval' v e
  where
    evalCdr' (SExpr [_, s]) = Right s
    evalCdr' (SExpr (_ : vs)) = Right $ SExpr vs
    evalCdr' v' = Left $ wrongArgFor "cdr" v' e
evalCdr s e = Left $ wrongArgFor "cdr" s e

evalQuote :: [SExpr] -> Either Error SExpr
evalQuote [v, Atom "()"] = Right v
evalQuote s = Left $ wrongArgFor "quote" s []

evalIsAtom :: [SExpr] -> Env -> Either Error SExpr
evalIsAtom [v, Atom "()"] e = toScheme . isAtom <$> eval' v e
  where
    isAtom (Atom _) = True
    isAtom _ = False
evalIsAtom s e = Left $ wrongArgFor "atom" s e

evalIsEq :: [SExpr] -> Env -> Either Error SExpr
evalIsEq [v1, v2, Atom "()"] e = do
    v1' <- eval' v1 e
    v2' <- eval' v2 e
    pure . toScheme $ eq v1' v2'
  where
    eq (Atom a1) (Atom a2) = a1 == a2
    eq _ _ = False
evalIsEq s e = Left $ wrongArgFor "eq?" s e

evalArith :: (Int -> Int -> Int) -> Int -> [SExpr] -> Env -> Either Error SExpr
evalArith op d args e =
    compute =<< traverse (`evalToInt` e) =<< chopNil args
  where
    compute [x] = Right . toScheme $ op d x
    compute (x : xs) = Right . toScheme $ foldl' op x xs
    compute _ = Left $ wrongArgFor "exp arithm" args e

evalToInt :: SExpr -> Env -> Either Error Int
evalToInt v e = toInt =<< eval' v e

evalBin :: ToScheme a => (Int -> Int -> a) -> [SExpr] -> Env -> Either Error SExpr
evalBin op [v1, v2, Atom "()"] e = do
    i1 <- evalToInt v1 e
    i2 <- evalToInt v2 e
    pure $ toScheme (op i1 i2)
evalBin _ s e = Left $ wrongArgFor "binary op" s e

evalLambda :: [SExpr] -> [SExpr] -> Env -> Either Error SExpr
evalLambda [SExpr params, body@(SExpr _), Atom "()"] args e = do
    e' <- traverse define =<< zipM (chopNil params) (chopNil args)
    fst <$> eval body (e' <> e)
  where
    zipM a1 a2 = liftM2 zip a1 a2
    define (x, v) = evalDefine' [x, v, Atom "()"] e
evalLambda _ s e = Left $ wrongArgFor "lambda" s e

evalDefine' :: [SExpr] -> Env -> Either Error (String, SExpr)
evalDefine' [Atom a, s, Atom "()"] e = (a,) <$> eval' s e
evalDefine' [SExpr (Atom a : args), body, Atom "()"] e =
    evalDefine' [Atom a, SExpr [Atom "lambda", SExpr args, body, Atom "()"], Atom "()"] e
evalDefine' s e = Left $ wrongArgFor "define" s e

evalDefine :: [SExpr] -> Env -> Either Error (SExpr, Env)
evalDefine s e = do
    e'@(_, v) <- evalDefine' s e
    pure (v, e' : e)

evalLet :: [SExpr] -> Env -> Either Error (SExpr, Env)
evalLet [SExpr defines, s, Atom "()"] e = do
    e' <- getAllEnvs =<< chopNil defines
    runEnv (eval' s) (e' <> e)
  where
    getAllEnvs = traverse (envFromDefine <=< toSExpr)
    envFromDefine x = evalDefine' x e
    toSExpr (SExpr s') = Right s'
    toSExpr _ = Left err
    err = wrongArgFor "let" s e
evalLet s e = Left $ wrongArgFor "let" s e

evalCond :: [SExpr] -> Env -> Either Error (SExpr, Env)
evalCond (SExpr [v1, v2, Atom "()"] : xs) e = do
    v1' <- eval' v1 e
    case v1' of
        Atom "#t" -> runEnv (eval' v2) e
        Atom "#f" -> evalCond xs e
        s -> Left $ wrongArgFor "cond" s e
evalCond s e = Left $ wrongArgFor "cond" s e

wrongArgFor :: Show a => String -> a -> Env -> Error
wrongArgFor name x e = WrongArgument name (show x) e

toInt :: SExpr -> Either Error Int
toInt s@(Atom a)
    | Just (i, _) <- runParser parseInt a = Right i
    | otherwise = Left $ IntConvert s
toInt s = Left $ IntConvert s

chopNil :: [SExpr] -> Either Error [SExpr]
chopNil x
    | Just x' <- safeInit x = Right x'
    | otherwise = Left $ NotAList (SExpr x)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [_] = Just []
safeInit (x : xs) = (:) x <$> safeInit xs
