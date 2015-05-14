module Language.Fw.AST
    ( module C
    , Kind(..)
    , Type(..)
    , Term(..)
    , Name, Exp, Ty
    , isValue
    , tytraverse
    ) where

import Control.Arrow (second)

import           Language.Lambda.Const as C

data Kind
    = Star
    | KnArr Kind Kind
    deriving (Show, Eq)

data Type a
    = TyVar a
    | TyArr (Type a) (Type a)
    | TyRec [(a, Type a)]
    | TyForall a Kind (Type a)
    | TyExists a Kind (Type a)
    | TyLam a Kind (Type a)
    | TyApp (Type a) (Type a)
    | TyInt
    | TyStr
    | TyBool
    deriving (Show, Eq)

tytraverse :: (a -> Type a) -> Type a -> Type a
tytraverse f = go
  where
    go (TyVar x) = f x
    go (TyArr t s) = TyArr (go t) (go s)
    go (TyRec fields) = TyRec (map (second go) fields)
    go (TyForall x k t) = TyForall x k (go t)
    go (TyExists x k t) = TyExists x k (go t)
    go (TyLam x k t) = TyLam x k (go t)
    go (TyApp s t) = TyApp (go s) (go t)
    go t = t

data Term a
    = Var a
    | Lam a (Type a) (Term a)
    | App (Term a) (Term a)
    | Rec [(a, Term a)]
    | Sel a (Term a) -- Select label a from a record
    | TyLamTerm a Kind (Term a) -- Lambda parameterized by a type which yields an expression
    | TyAppTerm (Term a) (Type a) -- Apply a TyLamTerm to a Type
    | Pack (Type a) (Term a) (Type a)
    | Unpack a a (Term a) (Term a)
    | Let a (Term a) (Term a)
    | Lit Literal
    | BinOp Op (Term a) (Term a)
    deriving (Show, Eq)

type Name = String
type Exp = Term Name
type Ty = Type Name

isValue :: Term a -> Bool
isValue (Lit _) = True
isValue Lam{} = True
isValue TyLamTerm{} = True
isValue (Rec fields) = all (isValue . snd) fields
isValue (Pack _ e _) = isValue e
isValue _ = False
