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

getBinOp :: Floating a => String -> (a -> a -> a)
getBinOp op = case op of
    "+" -> (+)
    "*" -> (*)
    "^" -> (**)

--------------------------------------------------- STORE

type Store = [(String, Float)]

initStore :: Store
initStore = []

removeFromStore :: Store -> String -> Store
removeFromStore store var =
    let maybePos = findVar store var 0 in
    case maybePos of
        Nothing -> store
        Just pos -> take pos store ++ drop (pos + 1) store

addToStore :: Store -> String -> Float -> Store
addToStore store var val =
    let maybePos = findVar store var 0 in
    case maybePos of
        Nothing -> (var, val):store
        Just pos -> take pos store ++ [(var, val)] ++ drop (pos + 1) store

findVar :: Store -> String -> Int -> Maybe Int
findVar store var pos = case store of
    [] -> Nothing
    [(x,y)] -> if x == var then Just pos else Nothing
    (x:xs) -> if fst x == var then Just pos else findVar xs var (pos + 1)
    
valueVar :: Store -> String -> Maybe Float
valueVar store var =
    let maybePos = findVar store var 0 in
    case maybePos of
        Nothing -> Nothing
        Just pos -> Just (snd (store !! pos))

--------------------------------------------------- EVAL

eval :: Store -> Expr -> Maybe Float
eval store expr = case expr of
    Const c -> Just c
    Variable v -> valueVar store v
    Uni o e -> let r = eval store e in
               let op = getUniOp o in
               case r of
                    Nothing -> Nothing
                    Just v -> Just (op v)
    Bin o e1 e2 -> let r1 = eval store e1 in
                   let r2 = eval store e2 in
                   let op = getBinOp o in
                   case (r1, r2) of
                       (Just v1, Just v2) -> Just (op v1 v2)
                       (_, _) -> Nothing