module Language.Lambda.Parser where

import           Control.Applicative   ((<$>))

import           Data.List             (foldl')

import           Text.Parsec
import           Text.Parsec.Expr      as E
import           Text.Parsec.String    (Parser, parseFromFile)

import           Language.Lambda.AST
import           Language.Lambda.Lexer

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

manyLambda :: [String] -> Exp -> Exp
manyLambda xs e = foldl' (flip Lam) e (reverse xs)

lambdaExpr :: Parser Exp
lambdaExpr = do
    lambda
    xs <- many1 identifier
    _ <- dot
    e <- expr
    return $ manyLambda xs e
    <?> "Lambda"

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

recordField :: Parser (String, Exp)
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
    <?> "Primary"

factor :: Parser Exp
factor = try selection <|> primary <?> "Factor"

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

assign :: Parser (String, Exp)
assign = do
    name <- identifier
    equals
    e <- expr
    _ <- semi
    return (name, e)
    <?> "Assignment"

assignments :: Parser [(String, Exp)]
assignments = allOf (many1 assign)
    <?> "Assignments"

parseProg :: String -> [(String, Exp)]
parseProg t = case parse assignments "" t of
    Left err -> error (show err)
    Right asgn -> asgn

parseProgFromFile :: FilePath -> IO [(String, Exp)]
parseProgFromFile fp = do
    result <- parseFromFile assignments fp
    case result of
        Left err -> do
            print err
            error $ "Failed to parse " ++ show fp
        Right defs -> return defs
