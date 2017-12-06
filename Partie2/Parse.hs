module Parse where

import Text.Parsec
import Data.Char
import Expression
import Data.Functor.Identity
import Text.ParserCombinators.Parsec.Expr

type  Parser a = Parsec String () a

lpar = char '('
rpar = char ')'
dot = char '.'

------------------------ (EXPR)

expr_par :: Parser Expr
expr_par = do
    lpar
    e <- expr
    rpar
    return e

------------------------ CONST

number :: Parser String
number = many1 digit

num :: Parser Float
num = do
    n <- number
    return (read n)
    
dec :: Parser Float
dec = do
    n1 <- number
    dot
    n2 <- number
    return (read (n1 ++ "." ++ n2))


readConstante = Text.Parsec.try dec <|> num

constante :: Parser Expr
constante = do 
    x <- readConstante
    return (Const x)

------------------------ VAR

var :: Parser Expr
var = do
    head <- letter
    tail <- many letter
    return (Variable (head:tail))

------------------------ OPERATOR

table = [   [   prefix "-" (Uni "-"),
                prefix "sin" (Uni "sin"),
                prefix "cos" (Uni "cos"),
                prefix "acos" (Uni "acos")
            ],
            [binary "*" (Bin "*") AssocLeft, binary "^" (Bin "^") AssocLeft, binary "/" (Bin "/") AssocLeft],
            [binary "+" (Bin "+") AssocLeft]
        ]

binary name fun assoc = Infix (do{ string name; return fun }) assoc
prefix name fun = Prefix (do{ string name; return fun })

------------------------ TERM

term = Text.Parsec.try expr_par <|> constante <|> var

-------------------------------------------parseExpression

expr = buildExpressionParser table term

parseExpression :: String -> Either String Expr
parseExpression s =
    let r = parse expr "" (filter (not . isSpace) s) in
    case r of
        Right e -> Right e
        Left e -> Left "Il y a une erreur dans l'expression"