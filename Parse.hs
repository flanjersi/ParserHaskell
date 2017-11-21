module Parse where

import Text.Parsec
import Data.Char
import Expression
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


type  Parser a = Parsec String () a


lpar = char '('
rpar = char ')'

term = expr_par <|> constante <|> var <?> "bonjour" 

expr_par = do
    lpar
    e <- expr
    rpar
    return e

-- Fonction venant de : https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
-- number = Text.Parsec.try (many1 digit)
-- decima = option "" $ (:) <$> char '.' <*> number

-- readConstante = fmap rd $ (++) <$> number <*> decima
        -- where rd = read :: String -> Float

-- constante = do 
    -- x <- readConstante
    -- return (Const x)

var = do
    head <- letter
    tail <- many letter
    return (Variable (head:tail))
    
table = [   [prefix "-" (Uni "-")],
            [binary "*" (Bin "*") AssocLeft, binary "^" (Bin "^") AssocLeft ] ,
            [binary "+" (Bin "+") AssocLeft]
        ]

binary  name fun assoc = Infix (do{ string name; return fun }) assoc

prefix  name fun = Prefix (do{ string name; return fun })

expr = buildExpressionParser table term


parseExpression :: String -> Maybe Expr
parseExpression s =
    let r = parse expr "" (filter (not . isSpace) s) in
    case r of
        Right e -> Just e
        Left e -> Nothing

{-
------------------------ Variables pour le parseur --------------------------------
minusChar = string "-"
plusChar = string "+"
multChar = string "*"
powChar = string "^"
lpar = char '('
rpar = char ')'
-- dotChar = char '.'
-- sinString = string "sin"

uniChar = minusChar -- <|> sinString
binChar = plusChar <|> multChar <|> powChar
---------------------------------- Parseur ----------------------------------------

expr_no_op :: Parser Expr
expr_no_op = try uni <|> constante <|> var

expr ::  Parser Expr
expr = bin <|> expr_no_op <|> expr_par

expr_par :: Parser Expr
expr_par = do
    lpar
    e <- expr
    rpar
    return e

bin :: Parser Expr
bin = do
    e1 <- expr_no_op
    op <- binChar
    e2 <- expr
    return (Bin op e1 e2)
    
uni :: Parser Expr
uni = do
    op <- uniChar
    e <- expr
    return (Uni op e)
    
-- Fonction venant de : https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
number = many1 digit
decimal = option "" $ (:) <$> char '.' <*> number

readConstante :: Parser Float
readConstante = fmap rd $ (++) <$> number <*> decimal
        where rd = read :: String -> Float

constante :: Parser Expr
constante = do
    x <- readConstante
    return (Const x)
{-
ci :: Parser Expr
ci = do
    i <- oneOf "123456789"
    nt <- many digit
    return (Const (read (i:nt)))

cf :: Parser Expr
cf = do
    i <- oneOf "123456789"
    nt <- many digit
    f <- dotChar
    l <- oneOf "0123456789"
    oat <- many digit
    return (Const (read ((i:nt)++(f:l:oat))))
    

constante :: Parser Expr
constante = cf
-}

var :: Parser Expr
var = do
    head <- letter
    tail <- many letter
    return (Variable (head:tail))

------------------------------ parseExpression ------------------------------------

parseExpression :: String -> Maybe Expr
parseExpression s =
    let r = parse expr "" (filter (not . isSpace) s) in
    case r of
        Right e -> Just e
        Left e -> Nothing
-}