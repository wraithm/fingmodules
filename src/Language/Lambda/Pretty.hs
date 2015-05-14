module Language.Lambda.Pretty where

import           Data.Monoid         (mempty)

import           Text.PrettyPrint

import           Language.Lambda.AST

prettyProgram :: [(Name, Exp)] -> String
prettyProgram = render . semiSep . map prettyDef

semiSep :: [Doc] -> Doc
semiSep = vcat . punctuate semi

spaceSep :: [String] -> Doc
spaceSep = sep . map text

dot :: Doc
dot = text "."

angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

prettyDef :: (Name, Exp) -> Doc
prettyDef (name, e) =
    text name <+> equals <+> prettyExp e

prettyOp :: Op -> Doc
prettyOp Add = text "+"
prettyOp Mul = text "*"
prettyOp Sub = text "-"
prettyOp Div = text "/"

prettyLit :: Literal -> Doc
prettyLit (IntConst i) = int i
prettyLit (StrConst s) = doubleQuotes (text s)
prettyLit (BoolConst t) = text (if t then "True" else "False")

isLam :: Term a -> Bool
isLam (Lam _ _) = True
isLam _ = False

prettyExp :: Exp -> Doc
prettyExp (Var x) = text x
prettyExp (Lit l) = prettyLit l
prettyExp (App e1 e2) =
    parensIf (isLam e1) (prettyExp e1) <+>
    parensIf (not $ isAtomic e2) (prettyExp e2)
  where
    isAtomic (Var _) = True
    isAtomic (Lit _) = True
    isAtomic (Sel _ _) = True
    isAtomic (Rec _) = True
    isAtomic _ = False

    parensIf p doc | p = parens doc | otherwise = doc
prettyExp (Let x e body) =
    text "let" <+>
        text x <+> equals <+> prettyExp e <+>
    text "in" <+>
        prettyExp body
prettyExp (Lam x e) =
    text "\\" <> text x <> terminator e <+>
    nextLam e
prettyExp (Rec fields) = braces prettyFields
  where
    prettyFields = commaSep $ map prettyField fields

    commaSep :: [Doc] -> Doc
    commaSep = vcat . map (nest 2) . punctuate comma

    prettyField (l, e) = text l <+> equals <+> prettyExp e
prettyExp (Sel l e) = prettyExp e <> dot <> text l
prettyExp (BinOp op e1 e2) = prettyExp e1 <+> prettyOp op <+> prettyExp e2
prettyExp (Closure env e) =
    text "Closure" <>
    angles (vcat (map prettyDef env) <> comma <+> prettyExp e)

nextLam :: Exp -> Doc
nextLam e
    | isLam e = noLam e
    | otherwise = prettyExp e

terminator :: Exp -> Doc
terminator e
    | isLam e = mempty
    | otherwise = dot

noLam :: Exp -> Doc
noLam (Lam x e) = text x <> terminator e <+> nextLam e
noLam e = prettyExp e
