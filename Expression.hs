module Expression where

------------------------ Expr ---------------------------

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

------------------------ Store --------------------------------

type Store = [(String, Float)]

initStore :: Store
initStore = []

addToStore :: Store -> String -> Float -> Store
addToStore s var val = (var, val):s

findVar :: Store -> String -> Maybe Float
findVar store var = case store of
    [] -> Nothing
    [(x,y)] -> if x == var then Just y else Nothing
    (x:xs) -> if fst x == var then Just (snd x) else findVar xs var

------------------------ Eval ---------------------------------

eval :: Store -> Expr -> Maybe Float
eval s expr = case expr of
    Const c -> Just c
    Variable v -> findVar s v
    Uni o e -> let r = eval s e in
               let op = getUniOp o in
               case r of
                    Nothing -> Nothing
                    Just v -> Just (op v)
    Bin o e1 e2 -> let r1 = eval s e1 in
                   let r2 = eval s e2 in
                   let op = getBinOp o in
                   case (r1, r2) of
                       (Just v1, Just v2) -> Just (op v1 v2)
                       (_, _) -> Nothing