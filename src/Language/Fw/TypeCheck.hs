module Language.Fw.TypeCheck
    ( Context(..)
    , prettyContext
    , lookupKind, lookupType
    , tyEq
    , kindOf
    , typeOf
    ) where

import           Control.Arrow      (second)

import           Text.PrettyPrint

import           Language.Fw.AST
import           Language.Fw.Pretty

-- DANGER
import Unsafe.Coerce (unsafeCoerce)
-- DANGER

data Context a
    = Empty
    | KnBind a Kind (Context a)
    | TyBind a (Type a) (Context a)
    deriving Eq

dropQuotes :: String -> String
dropQuotes [] = []
dropQuotes (_:xs) = init xs

instance Show a => Show (Context a) where
    show Empty = ""
    show (KnBind x k ctx) = dropQuotes (show x) ++ " :: " ++ show k ++ "\n" ++ show ctx
    show (TyBind x t ctx) = dropQuotes (show x) ++ " : " ++ show t ++ "\n" ++ show ctx

prettyContext :: Context Name -> String
prettyContext = render . vcat . prettyCtx
  where
    prettyCtx Empty = []
    prettyCtx (KnBind x k ctx) =
        text x <+> text "::" <+> prettyKind k : prettyCtx ctx
    prettyCtx (TyBind x t ctx) =
        text x <+> text ":" <+> prettyType t : prettyCtx ctx

lookupType :: Eq a => a -> Context a -> Maybe (Type a)
lookupType _ Empty = Nothing
lookupType x (KnBind _ _ ctx) = lookupType x ctx
lookupType x (TyBind y ty ctx)
    | x == y = Just ty
    | otherwise = lookupType x ctx

lookupKind :: Eq a => a -> Context a -> Maybe Kind
lookupKind _ Empty = Nothing
lookupKind x (TyBind _ _ ctx) = lookupKind x ctx
lookupKind x (KnBind y kn ctx)
    | x == y = Just kn
    | otherwise = lookupKind x ctx

kindError :: Show a => a -> Context a -> Kind
kindError x env = error $
    "Can't find kind of " ++ show x ++ " in context:\n" ++ show env

kindOf :: (Show a, Eq a) => Context a -> Type a -> Kind
kindOf env (TyVar x)
    | Just k <- lookupKind x env = k
    | otherwise = kindError x env
kindOf env (TyForall x k ty)
    | checkKindStar env' ty = Star
    | otherwise = error "Kind * expected."
  where
    env' = KnBind x k env
kindOf env (TyLam x k1 ty) = KnArr k1 k2
  where
    env' = KnBind x k1 env
    k2 = kindOf env' ty
kindOf env (TyApp t1 t2) = checkKindApp k1
  where
    k1 = kindOf env t1
    k2 = kindOf env t2

    checkKindApp (KnArr k1' k2')
        | k2 == k1' = k2'
        | otherwise = error "Parameter kind mismatch."
    checkKindApp _ = error "Arrow kind expected."
kindOf env (TyArr t1 t2)
    | checkKindStar env t1 && checkKindStar env t2 = Star
    | otherwise = error "Kind * expected."
kindOf env (TyExists x k ty)
    | checkKindStar env' ty = Star
    | otherwise = error "Kind * expected."
  where
    env' = KnBind x k env
kindOf env (TyRec fields)
    | fieldsKindStar fields = Star
    | otherwise = error "Kind * expected."
  where
    fieldsKindStar = all (checkKindStar env . snd)
kindOf _ TyInt = Star
kindOf _ TyStr = Star
kindOf _ TyBool = Star

checkKindStar :: (Show a, Eq a) => Context a -> Type a -> Bool
checkKindStar env ty = kindOf env ty == Star

typeError :: Show a => a -> Context a -> Type a
typeError x env = error $ 
    "Can't find type of " ++ show x ++ " in context:\n" ++ show env

tyEq :: (Show a, Eq a) => Context a -> Type a -> Type a -> Bool
tyEq _ TyInt TyInt = True
tyEq _ TyStr TyStr = True
tyEq _ TyBool TyBool = True
tyEq env (TyRec f1) (TyRec f2) =
    length f1 == length f2 &&
    all isInF1 f2
  where
    isInF1 (l,t2)
        | Just t1 <- lookup l f1 = tyEq env t1 t2
        | otherwise = False
tyEq env (TyArr s1 s2) (TyArr t1 t2) =
    tyEq env s1 t1 && tyEq env s2 t2
tyEq env (TyApp s1 s2) (TyApp t1 t2) =
    tyEq env s1 t1 && tyEq env s2 t2
{-
tyEq env (TyVar x) t
    | Just k <- lookupKind x env = k == kindOf env t
tyEq env s (TyVar x)
    | Just k <- lookupKind x env = kindOf env s == k
-}
tyEq _ (TyVar x) (TyVar y) = x == y
tyEq env (TyForall x j s) (TyForall _ k t) =
    j == k &&
    tyEq env' s t
  where
    env' = KnBind x j env
tyEq env (TyExists x j s) (TyExists _ k t) =
    j == k &&
    tyEq env' s t
  where
    env' = KnBind x j env
-- TODO TyLam
tyEq env (TyLam _ j s) (TyLam _ k t) =
    j == k &&
    tyEq env s t
tyEq _ _ _ = False

tySubst :: Eq a => a -> Type a -> Type a -> Type a
tySubst x t = tytraverse checkVar
  where
    checkVar y
        | x == y = t
        | otherwise = TyVar y

typeOf :: (Show a, Eq a) => Context a -> Term a -> Type a
typeOf env (Var x)
    | Just t <- lookupType x env = t
    | otherwise = typeError x env
typeOf env (Lam x ty1 e)
    | checkKindStar env ty1 = TyArr ty1 ty2
    | otherwise = error "Kind * expected."
  where
    env' = TyBind x ty1 env
    ty2 = typeOf env' e
typeOf env e@(App e1 e2) = checkTypeApp ty1
  where
    ty1 = typeOf env e1
    ty2 = typeOf env e2

    -- TODO Not simplifying type!
    checkTypeApp (TyArr ty1' ty2')
        | tyEq env ty2 ty1' = ty2'
        | otherwise = error $ unlines
            [ "Parameter type mismatch:"
            -- , show env
            -- DANGER ZONE
            , prettyContext (unsafeCoerce env)
            , "\n"
            , render . prettyExp $ unsafeCoerce e
            , "\n"
            -- DANGER ZONE
            , show e
            , show (ty1, ty1')
            , show (ty2, ty2') 
            , "\n"
            -- DANGER ZONE
            , render . prettyType $ unsafeCoerce ty1
            , "\n"
            , render . prettyType $ unsafeCoerce ty2
            , render . prettyType $ unsafeCoerce ty1' ]
            -- DANGER ZONE
    checkTypeApp e' = error . unlines $
        [ "\nArrow type expected:"
        -- , show env
        -- DANGER ZONE
        , prettyContext (unsafeCoerce env)
        -- DANGER ZONE
        , show e
        , show e'
        , show ty1
        , show ty2 ]

typeOf env (Rec fields) = TyRec (fmap (second (typeOf env)) fields)
typeOf env (Sel l r) = typeOfField t
  where
    t = typeOf env r

    typeOfField (TyRec fields)
        | Just tf <- lookup l fields = tf
        | otherwise = error $
            "Could not find field " ++ show l ++ " in record: " ++ show r
    typeOfField _ = error "Expected record."
typeOf env (TyLamTerm x k e) = TyForall x k t
  where
    env' = KnBind x k env
    t = typeOf env' e
typeOf env (TyAppTerm e ty2) = checkForallApp ty1
  where
    k = kindOf env ty2
    ty1 = typeOf env e

    checkForallApp (TyForall x k' ty2')
        | k == k' = tySubst x ty2 ty2' -- TODO No type substitution
        | otherwise = error "Type argument has wrong kind."
    checkForallApp _ = error "Universal type expected."

-- TODO check the type of the expression to make sure that it's not stupid.
typeOf env (Pack ty1 _ ty2)
    | checkKindStar env ty2 = checkType ty2
    | otherwise = error "Expected kind *."
  where
    checkType (TyExists _ k _)
        | kindOf env ty1 == k = ty2
    checkType _ = error "Existential type expected."
typeOf env (Unpack tyx x e1 e2) = checkType ty1
  where
    ty1 = typeOf env e1

    checkType (TyExists _ k ty') = ty2'
      where
        env' = TyBind x ty' (KnBind tyx k env)
        ty2' = typeOf env' e2
    checkType _ = error "Existential type expected."
typeOf _ (Lit (IntConst _)) = TyInt
typeOf _ (Lit (StrConst _)) = TyStr
typeOf _ (Lit (BoolConst _)) = TyBool
typeOf env (Let x e body) = typeOf env' body
  where
    ty = typeOf env e
    env' = TyBind x ty env
typeOf env (BinOp _ e1 e2)
    | TyInt <- typeOf env e1
    , TyInt <- typeOf env e2 = TyInt
    | otherwise = error "Expected Int type."
