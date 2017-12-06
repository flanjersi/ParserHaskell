module Expression where

--------------------------------------------------- EXPR 

data Expr = Const Float
    | Variable String
    | Uni String Expr
    | Bin String Expr Expr
    deriving (Show, Eq)

--Helpers
getUniOp :: Floating a => String -> (a -> a)
getUniOp op = case op of
    "-" -> negate
    "sin" -> sin
    "cos" -> cos
    "acos" -> acos

getBinOp :: Floating a => String -> (a -> a -> a)
getBinOp op = case op of
    "+" -> (+)
    "*" -> (*)
    "^" -> (**)
    "/" -> (/)

--------------------------------------------------- STORE

type Store = [(String, Float)]

initStore :: Store
initStore = []

removeFromStore :: Store -> String -> Store
removeFromStore store var =
    let maybePos = findVar store var 0 in
    case maybePos of
        Left ex -> store
        Right pos -> take pos store ++ drop (pos + 1) store

addToStore :: Store -> String -> Float -> Store
addToStore store var val =
    let maybePos = findVar store var 0 in
    case maybePos of
        Left ex -> (var, val):store
        Right pos -> take pos store ++ [(var, val)] ++ drop (pos + 1) store

findVar :: Store -> String -> Int -> Either String Int
findVar store var pos = case store of
    [] -> Left ("La variable " ++ var ++  " n'existe pas")
    (x:xs) -> if fst x == var then Right pos else findVar xs var (pos + 1)
    
valueVar :: Store -> String -> Either String Float
valueVar store var =
    let maybePos = findVar store var 0 in
    case maybePos of
        Left ex -> Left ex
        Right pos -> Right (snd (store !! pos))

--------------------------------------------------- EVAL

eval :: Store -> Expr -> Either String Float
eval store expr = case expr of
    Const c -> Right c
    Variable v -> valueVar store v
    Uni o e -> let r = eval store e in
               let op = getUniOp o in
               case r of
                    Left ex -> Left ex
                    Right v -> Right (op v)
    Bin o e1 e2 -> let r1 = eval store e1 in
                   let r2 = eval store e2 in
                   let op = getBinOp o in
                   case (r1, r2) of
                       (Right v1, Right v2) -> Right (op v1 v2)
                       (_, _) -> Left "Une variable n'a pas été trouvé"