module Language.Lambda.Eval where

import           Control.Arrow       (second)

import           Language.Lambda.AST

evalOp :: Integral a => Op -> a -> a -> a
evalOp Add = (+)
evalOp Mul = (*)
evalOp Sub = (-)
evalOp Div = div

eval :: Environment -> Exp -> Exp
eval env (Var x)
    | Just v <- lookup x env = eval env v
    | otherwise = runtimeError x env
eval env e@(Lam _ _) = Closure env e
eval env (BinOp op e1 e2)
    | Lit (IntConst i1) <- eval env e1
    , Lit (IntConst i2) <- eval env e2 = Lit (IntConst (evalOp op i1 i2))
    | otherwise = error $
        "Can't apply " ++ show op ++ " to " ++ show e1 ++ " and " ++ show e2
eval env (App e1 e2) =
    case eval env e1 of
        Closure env' (Lam x e) ->
            eval ((x, eval env e2) : env') e
        _ -> error $
            "Wrong type for application: " ++ show (e1, e2)
eval env (Let x e body) = eval env' body
  where env' = (x, eval env e) : env
eval _ (Closure env' e) = eval env' e
eval env (Sel l e)
    | Rec fields <- eval env e
    , Just v <- lookup l fields = eval env v
    | otherwise = error $
        "Cannot find field " ++ show l ++ " in expression: " ++ show e
eval env (Rec fields) = Rec (map (second (eval env)) fields)
eval _ e = e

runtimeError :: Show a => a -> Environment -> Term a
runtimeError x env = error $
    "Could not find variable " ++ show x ++ " in: " ++ show env

{-
isValue :: Term a -> Bool
isValue (Lit _) = True
isValue (Rec _) = True
isValue (Lam _ _) = True
isValue _ = False

-- import Bound

nf :: (Eq a, Show a) => Term a -> Term a
nf (Lam b) = Lam $ toScope $ nf $ fromScope b
nf (App f x) = case whnf f of
    Lam b -> nf (instantiate1 x b)
    f' -> App (nf f') (nf x)
nf (Let e b) = nf (instantiate1 e b)
nf (Sel l r) = case whnf r of
    Rec fields | Just v <- lookup l fields -> nf v
    _ -> error $ "Can't find field " ++ show l ++ " in: " ++ show r
nf (Rec fields) = Rec (map (both nf) fields)
  where both f (x, y) = (f x, f y)
nf e = e

whnf :: (Eq a, Show a) => Term a -> Term a
whnf (App f x) = case whnf f of
    Lam b -> whnf (instantiate1 x b)
    f' -> App f' x
whnf (Let e b) = whnf (instantiate1 e b)
whnf (Sel l r) = case whnf r of
    Rec fields | Just v <- lookup l fields -> whnf v
    _ -> error $ "Can't find field " ++ show l ++ " in: " ++ show r
whnf e = e
-}

{-
eval1 :: Environment -> Exp -> Exp
eval1 env (Var x)
    | Just v <- lookup x env = v
    | otherwise = runtimeError x env
eval1 env e@(Lam _ _) = Closure env e
eval1 env (App e1 e2) = applyFunction v1
  where
    v1 = eval env e1

    applyFunction (Closure env' (Lam x e)) =
        eval env'' e
      where
        env'' = put x (eval env e2) env'
    applyFunction (Lam x e) = eval env' e
      where
        env' = put x (eval env e2) env
    applyFunction e = error $ "Wrong type: " ++ show e
eval1 env (Let x e body) = eval env' body
  where env' = put x (eval env e) env
eval1 env (Closure env' e) = eval (env' ++ env) e
eval1 env (Sel l e)
    | Rec fields <- eval env e
    , Just v <- lookup l fields = v
    | otherwise = error $ "Cannot find field " ++ show l ++ " in expression: " ++ show e
eval1 _ e = e

eval :: Environment -> Exp -> Exp
eval _ e | isValue e = e
eval env e = eval env (eval1 env e)
-}
