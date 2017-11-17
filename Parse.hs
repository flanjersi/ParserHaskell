module Parse where

import  Text.Parsec
import Data.Char
import Expression
import Data.Functor.Identity
type  Parser a = Parsec String () a


------------------------ Variables pour le parseur --------------------------------
minusChar = char '-'
plusChar = char '+'
multChar = char '*'
powChar = char '^'
lpar = char '('
rpar = char ')'

uniChar = minusChar
binChar = plusChar <|> multChar <|> powChar
---------------------------------- Parseur ----------------------------------------

expr ::  Parser Expr
expr = try bin <|> uni <|> constante <|> var
bin :: Parser Expr
bin = do
    lpar
    e1 <- expr
    op <- binChar
    e2 <- expr
    rpar
    return (Bin (getBinOp op) e1 e2)
    
uni :: Parser Expr
uni = do
    op <- uniChar
    e <- expr
    return (Uni (getUniOp op) e)
    
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

var :: Parser Expr
var = do
    s <- many letter
    return (Variable s)

------------------------------ parseExpression ------------------------------------

parseExpression :: String -> Maybe Expr
parseExpression s =
    let r = parse expr "" (filter (not . isSpace) s) in
    case r of
        Right e -> Just e
        Left e -> Nothing
