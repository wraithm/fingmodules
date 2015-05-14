module Language.Fw.Pretty where

import           Text.PrettyPrint

import           Language.Fw.AST

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

prettyKind :: Kind -> Doc
prettyKind Star = text "*"
prettyKind (KnArr k1 k2) = prettyKind k1 <+> text "->" <+> prettyKind k2

prettyAnn :: (a -> Doc) -> Name -> a -> Doc
prettyAnn p l a = text l <> colon <> p a

parensIf :: Bool -> Doc -> Doc
parensIf p doc | p = parens doc | otherwise = doc

kindAnnotation :: Name -> Kind -> Doc
kindAnnotation x k
    | k == Star = text x
    | otherwise = prettyAnn prettyKind x k

prettyType :: Ty -> Doc
prettyType (TyVar x) = text x
prettyType (TyArr t1 t2) = 
    parensIf (isTyArr t1) (prettyType t1) <+> text "->" <+> prettyType t2
  where
    isTyArr (TyArr _ _) = True
    isTyArr _ = False
prettyType (TyRec fields) = braces $ prettyFields colon prettyType fields
prettyType (TyForall x k t) = text "forall" <+> kindAnnotation x k <> dot <+> prettyType t
prettyType (TyExists x k t) = text "exists" <+> kindAnnotation x k <> dot <+> prettyType t
prettyType (TyLam x k t) = text "\\" <+> prettyAnn prettyKind x k <> dot <+> prettyType t
prettyType (TyApp t1 t2) =
    parensIf (isLamTy t1) (prettyType t1) <+>
    parensIf (not $ isAtomic t2) (prettyType t2)
  where
    isLamTy TyLam{} = True
    isLamTy _ = False

    isAtomic (TyVar _) = True
    isAtomic TyInt = True
    isAtomic TyStr = True
    isAtomic TyBool = True
    isAtomic (TyRec _) = True
    isAtomic _ = False
prettyType TyInt = text "Int"
prettyType TyStr = text "String"
prettyType TyBool = text "Bool"

prettyFields :: Doc -> (a -> Doc) -> [(Name, a)] -> Doc
prettyFields s p = commaSep . map (prettyField s p)

prettyField :: Doc -> (a -> Doc) -> (Name, a) -> Doc
prettyField s p (l, e) = text l <+> s <+> p e

commaSep :: [Doc] -> Doc
commaSep = vcat . map (nest 2) . punctuate comma

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
prettyExp (Let x e body) = vcat
    [ text "let" <+>
        text x <+> equals <+> prettyExp e <+>
        text "in"
    , prettyExp body
    ]
prettyExp (Lam x t e) =
    text "\\" <> prettyAnn prettyType x t <> terminator e <+> nextLam e
prettyExp (Rec fields) = braces (prettyFields equals prettyExp fields)
prettyExp (Sel l e) = prettyExp e <> dot <> text l
prettyExp (Pack t e tas) = vcat
    [ text "pack" <>
        angles (prettyType t <> comma <+> prettyExp e)
    , text "as" <+>
        prettyType tas
    ]
prettyExp (Unpack t x e body) = vcat
    [ text "unpack" <> angles (text t <> comma <+> text x) <+> equals <+> prettyExp e <+> text "in"
    , prettyExp body
    ]
prettyExp (TyLamTerm x k e) =
    text "\\" <> kindAnnotation x k <> terminator e <+> 
    nextLam e
prettyExp (TyAppTerm f t) =
    parensIf (isLam f) (prettyExp f) <+>
    brackets (prettyType t)
prettyExp (BinOp op e1 e2) = prettyExp e1 <+> prettyOp op <+> prettyExp e2

nextLam :: Exp -> Doc
nextLam e
    | isLam e = noLam e
    | otherwise = prettyExp e

isLam :: Term a -> Bool
isLam Lam{} = True
isLam TyLamTerm{} = True
isLam _ = False

terminator :: Exp -> Doc
terminator e
    | isLam e = comma
    | otherwise = dot

noLam :: Exp -> Doc
noLam (Lam x t e) = 
    prettyAnn prettyType x t <> terminator e <+> nextLam e
noLam (TyLamTerm x k e) =
    kindAnnotation x k <> terminator e <+> 
    nextLam e
noLam e = prettyExp e
