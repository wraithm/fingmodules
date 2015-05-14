module Language.Lambda.Lexer where

import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as P

lambdaDef = emptyDef
    { P.commentStart = "{-"
    , P.commentEnd = "-}"
    , P.commentLine = "--"
    , P.nestedComments = True
    , P.identStart = letter
    , P.identLetter = alphaNum <|> oneOf "_'"
    , P.opStart = P.opLetter lambdaDef
    -- , P.opLetter = oneOf "=+/*-~><&|\\"
    , P.opLetter = oneOf "=\\.*+-/"
    , P.reservedNames =
        [ "let", "in"
        , "True", "False"
        ]
    }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser lambdaDef

parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
integer    = P.integer lexer
strLiteral = P.stringLiteral lexer
operator   = P.operator lexer
reservedOp = P.reservedOp lexer
whitespace = P.whiteSpace lexer
dot        = P.dot lexer
semi       = P.semi lexer
braces     = P.braces lexer
brackets   = P.brackets lexer
angles     = P.angles lexer
commaSep   = P.commaSep lexer

lambda = reservedOp "\\"
equals = reservedOp "="

allOf p = do
    whitespace
    r <- p
    eof
    return r
