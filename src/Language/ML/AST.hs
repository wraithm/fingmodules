module Language.ML.AST where

import Language.Fw.AST as Fw

data MLType a
    = TyFw (Type a)
    | TyMod (Module a)
    deriving (Eq, Show)

data MLTerm a
    = FwTerm (Term a)
    | Mod (Module a)
    deriving (Eq, Show)

data Module a
    = MdVar a
    | MdBinds (Binding a)
    | MdSel a (Module a) -- Select a from a module
    | MdFunc a (Sig a) (Module a)
    | MdApp a a
    | MdSub a (Sig a)
    deriving (Eq, Show)

data Binding a
    = BdVal a (MLTerm a)
    | TyBind a (MLType a)
    | MdBind a (Module a)
    | SigBind a (Sig a)
    | BdInclude (Module a)
    | BdEmpty
    | Bindings (Binding a) (Binding a) -- semi-colon separation of bindings
    deriving (Eq, Show)

data Sig a
    = SigMod (Module a)
    | SigDecls (Decl a)
    | SigFunc a (Sig a) (Sig a)
    | WhereType (Sig a) a (MLType a)
    deriving (Eq, Show)

emptySig :: Sig a
emptySig = SigDecls DcEmpty

data Decl a
    = DcVal a (MLType a)
    | DcType a (MLType a)
    | DcTyKind a Kind
    | DcModule a (Sig a)
    | DcSig a (Sig a)
    | DcInclude (Sig a)
    | DcEmpty
    | Decls (Decl a) (Decl a) -- Semi-colon separation of decls
    deriving (Eq, Show)
