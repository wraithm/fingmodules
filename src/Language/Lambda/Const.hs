module Language.Lambda.Const where

data Literal
    = IntConst Int
    | StrConst String
    | BoolConst Bool
    deriving (Show, Eq)

data Op = Add | Mul | Sub | Div
    deriving (Show, Eq)
