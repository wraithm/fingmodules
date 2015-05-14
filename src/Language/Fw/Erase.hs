module Language.Fw.Erase where

import Control.Arrow (second)

import Language.Lambda.AST as L
import Language.Fw.AST as F

erase :: F.Term a -> L.Term a
erase (F.Var a) = L.Var a
erase (F.App f x) = L.App (erase f) (erase x)
erase (F.Lam x _ e) = L.Lam x (erase e)
erase (F.Rec fields) = L.Rec (map (second erase) fields)
erase (F.Sel l r) = L.Sel l (erase r)
erase (F.TyLamTerm _ _ e) = erase e
erase (F.TyAppTerm e _) = erase e
erase (F.Pack _ e _) = erase e
erase (F.Unpack _ x e b) = L.Let x (erase e) (erase b)
erase (F.Let x e b) = L.Let x (erase e) (erase b)
erase (F.BinOp op e1 e2) = L.BinOp op (erase e1) (erase e2)
erase (F.Lit x) = L.Lit (eraseLit x)
  where
    eraseLit (F.IntConst i) = L.IntConst i
    eraseLit (F.StrConst s) = L.StrConst s
    eraseLit (F.BoolConst b) = L.BoolConst b
