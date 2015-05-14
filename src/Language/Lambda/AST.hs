module Language.Lambda.AST 
    ( Name, Exp, Environment
    , module C
    , Term(..)
    ) where

import Language.Lambda.Const as C

{-
import           Data.Foldable
import           Data.Traversable

import           Control.Applicative
import           Control.Monad

import           Bound
import           Prelude.Extras
-}

type Name = String
type Exp = Term String
type Environment = [(Name, Exp)]

data Term a
    = Var a
    | Lit Literal
    | BinOp Op (Term a) (Term a)
    | Lam a (Term a)
    | App (Term a) (Term a)
    | Let a (Term a) (Term a)
    | Rec [(a, Term a)]
    | Sel a (Term a) -- Select label a from a record

    -- Runtime only
    | Closure [(a, Term a)] (Term a)
    deriving (Show, Eq)

{-
instance Applicative Term where
    pure = Var
    (<*>) = ap

instance Monad Term where
    return = Var
    Var a >>= f = f a
    Lit x >>= _ = Lit x
    Lam e >>= f = Lam (e >>>= f)
    App x y >>= f = App (x >>= f) (y >>= f)
    Let e b >>= f = Let (e >>= f) (b >>>= f)
    Rec fields >>= f = Rec (map (\(l,e) -> (l >>= f, e >>= f)) fields)
    Sel l x >>= f = Sel (l >>= f) (x >>= f)

instance Eq1 Term
instance Ord1 Term
instance Show1 Term
instance Read1 Term

lam :: Eq a => a -> Term a -> Term a
lam v b = Lam (abstract1 v b)

let_ :: Eq a => a -> Term a -> Term a -> Term a
let_ x e b = Let e (abstract1 x b)

record :: Eq a => [(a, Term a)] -> Term a
record fields = Rec (map abstractField fields)
  where abstractField (l, e) = (return l, e)

sel :: Eq a => a -> Term a -> Term a
sel l = Sel (return l)

($:) :: Term a -> Term a -> Term a
($:) = App

int :: Int -> Term a
int = Lit . IntConst

str :: String -> Term a
str = Lit . StrConst

bool :: Bool -> Term a
bool = Lit . BoolConst
-}
