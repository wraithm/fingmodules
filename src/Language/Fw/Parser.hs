module Language.Fw.Parser where

import           Control.Applicative   ((<$>))

import           Data.List             (foldl')

import           Text.Parsec
import           Text.Parsec.Expr      as E
import           Text.Parsec.String    (Parser, parseFromFile)

import           Language.Fw.AST
import           Language.Fw.Lexer

-- Kinds
star :: Parser Kind
star = reserved "*" >> return Star
    <?> "Star"

kindfactor :: Parser Kind
kindfactor = parens kind <|> star

kind :: Parser Kind
kind = E.buildExpressionParser kindTable kindfactor <?> "Kind"
  where
    kindTable = [[ Infix (arrow >> return KnArr) AssocRight ]]

kindAnn :: Parser (Name, Kind)
kindAnn = do
    x <- identifier
    k <- try $ do
        _ <- colon
        kind
     <|> return Star
    return (x, k)

-- Types
tyVar :: Parser Ty
tyVar = TyVar <$> identifier <?> "TyVar"

tyRec :: Parser Ty
tyRec = TyRec <$> braces (commaSep typeAnn)
    <?> "Record type"

typeAnn :: Parser (Name, Ty)
typeAnn = do
    l <- identifier
    _ <- colon
    t <- ty
    return (l, t)
    <?> "Record type field"

tyForall :: Parser Ty
tyForall = do
    reserved "forall"
    (x, k) <- kindAnn
    _ <- dot
    t <- ty
    return $ TyForall x k t

tyExists :: Parser Ty
tyExists = do
    reserved "exists"
    (x, k) <- kindAnn
    _ <- dot
    t <- ty
    return $ TyExists x k t

tyLam :: Parser Ty
tyLam = do
    lambda
    (x, k) <- kindAnn
    _ <- dot
    t <- ty
    return $ TyLam x k t

tyInt :: Parser Ty
tyInt = reserved "Int" >> return TyInt

tyStr :: Parser Ty
tyStr = reserved "String" >> return TyStr

tyBool :: Parser Ty
tyBool = reserved "Bool" >> return TyBool

typ :: Parser Ty
typ = parens ty
    <|> tyVar
    <|> tyRec
    <|> tyForall
    <|> tyExists
    <|> tyLam
    <|> tyInt <|> tyStr <|> tyBool

tyterm :: Parser Ty
tyterm = E.buildExpressionParser tyTable typ
  where
    tyTable = [[Infix tyArr AssocRight ]]
    tyArr = arrow >> return TyArr

ty :: Parser Ty
ty = foldl1 TyApp <$> many1 tyterm

-- Terms
var :: Parser Exp
var = Var <$> identifier <?> "Var"

intConst :: Parser Exp
intConst = Lit . IntConst . fromInteger <$> integer <?> "Int"

strConst :: Parser Exp
strConst = Lit . StrConst <$> strLiteral <?> "String"

boolConst :: Parser Exp
boolConst = Lit . BoolConst <$> (true <|> false) <?> "Bool"
  where
    true = reserved "True" >> return True
    false = reserved "False" >> return False

typeOrKindAnn :: Parser (Name, Either Kind Ty)
typeOrKindAnn = do
    x <- identifier
    ann <- try $ do
        _ <- colon
        Right <$> try ty <|> Left <$> kind
     <|> return (Left Star)
    return (x, ann)

manyLambda :: [(Name, Either Kind Ty)] -> Exp -> Exp
manyLambda xs e = foldl' addLam e (reverse xs)
  where
    addLam e' (x, Left k) = TyLamTerm x k e'
    addLam e' (x, Right t) = Lam x t e'

lambdaExpr :: Parser Exp
lambdaExpr = do
    lambda
    xs <- commaSep typeOrKindAnn
    _ <- dot
    e <- expr
    return $ manyLambda xs e

letExpr :: Parser Exp
letExpr = do
    reserved "let"
    name <- identifier
    equals
    e <- expr
    reserved "in"
    body <- expr
    return $ Let name e body
    <?> "Let"

pack :: Parser Exp
pack = do
    reserved "pack"
    (t, e) <- angles (do { t <- ty; _ <- comma; x <- expr; return (t, x) })
    reserved "as"
    ast <- ty
    return (Pack t e ast)
    <?> "Pack"

unpack :: Parser Exp
unpack = do
    reserved "unpack"
    (t,x) <- angles (do { t <- identifier; _ <- comma; x <- identifier; return (t, x) })
    equals
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (Unpack t x e1 e2)
    <?> "Unpack"

selection :: Parser Exp
selection = do
    e <- primary
    _ <- dot
    l <- identifier
    return (Sel l e)
    <?> "Record selection"

record :: Parser Exp
record = Rec <$> braces (commaSep recordField)
    <?> "Record"

recordField :: Parser (Name, Exp)
recordField = do
    l <- identifier
    equals
    e <- expr
    return (l, e)
    <?> "Record field"

primary :: Parser Exp
primary = parens expr
    <|> var
    <|> intConst
    <|> strConst
    <|> boolConst
    <|> lambdaExpr
    <|> letExpr
    <|> record
    <|> pack
    <|> unpack
    <?> "Primary"

tyApp :: Parser Exp
tyApp = do
    e <- primary
    t <- brackets ty
    return (TyAppTerm e t)
    <?> "Type application"

factor :: Parser Exp
factor = try selection <|> try tyApp <|> primary 
    <?> "Factor"

term :: Parser Exp
term = E.buildExpressionParser table factor <?> "Term"
  where 
    table =
        [ [ binOp "*" Mul, binOp "/" Div ]
        , [ binOp "+" Add, binOp "-" Sub ]
        ]

    binOp sym op = Infix (reservedOp sym >> return (BinOp op)) AssocLeft

expr :: Parser Exp
expr = foldl1 App <$> many1 term
    <?> "Expression"

assign :: Parser (Name, Exp)
assign = do
    name <- identifier
    equals
    e <- expr
    _ <- semi
    return (name, e)
    <?> "Assignment"

assignments :: Parser [(Name, Exp)]
assignments = allOf (many1 assign)
    <?> "Assignments"

parseProg :: String -> [(Name, Exp)]
parseProg t = case parse assignments "" t of
    Left err -> error (show err)
    Right asgn -> asgn

parseProgFromFile :: FilePath -> IO [(Name, Exp)]
parseProgFromFile fp = do
    result <- parseFromFile assignments fp
    case result of
        Left err -> do
            print err
            error $ "Failed to parse " ++ show fp
        Right defs -> return defs
