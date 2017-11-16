import  Text.Parsec
import Data.Char
import Expression
type  Parser a = Parsec String () a

minusChar = char '-'
plusChar = char '+'
multChar = char '*'
powChar = char '^'
lpar = char '('
rpar = char ')'

expr ::  Parser Expr
expr = try add <|> mult <|> pow <|> minus <|> var

-- Fonction venant de : https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec

number = many1 digit
plus   = char '+' *> number
minus2  = (:) <$> char '-' <*> number
integer = plus <|> minus2 <|> number
decimal = option "" $ (:) <$> char '.' <*> number

readConstante :: Parser Float
readConstante = fmap rd $ (++) <$> integer <*> decimal
        where rd = read :: String -> Float

constante :: Parser Expr
constante = do
    x <- readConstante
    return (Const x)

var :: Parser Expr
var = do
    s <- many letter
    return (Variable s)

minus :: Parser Expr
minus = do
    minusChar
    e <- expr
    return (Minus e)

add :: Parser Expr
add = do
    lpar
    e1 <- expr
    plusChar
    e2 <- expr
    rpar
    return (Add e1 e2)

mult :: Parser Expr
mult = do
    lpar
    e1 <- expr
    multChar
    e2 <- expr
    rpar
    return (Mult e1 e2)

pow :: Parser Expr
pow = do
    lpar
    e1 <- expr
    powChar
    e2 <- expr
    rpar
    return (Pow e1 e2)

parseExpression :: String -> Maybe Expr
parseExpression s =
    let r = parse expr "" (filter (not . isSpace) s) in
    case r of
        Right e -> Just e
        Left e -> Nothing
    