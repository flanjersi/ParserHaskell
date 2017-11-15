module Expression where
------------------------ Expr ---------------------------

data Expr = Const Float
    | Variable String
    | Minus Expr
    | Add Expr Expr
    | Mult Expr Expr
    | Pow Expr Expr
    deriving (Show)

------------------------ Store --------------------------------

type Store = [(String, Float)]

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
    Minus e -> let r = eval s e in
               case r of
                   Nothing -> Nothing
                   Just v -> Just (-v)
    Add e1 e2 -> let r1 = eval s e1 in
                 let r2 = eval s e2 in
                 case (r1, r2) of
                     (Just v1, Just v2) -> Just ((+) v1 v2)
                     (_, _) -> Nothing
    Mult e1 e2 -> let r1 = eval s e1 in
                 let r2 = eval s e2 in
                 case (r1, r2) of
                     (Just v1, Just v2) -> Just ((*) v1 v2)
                     (_, _) -> Nothing
    Pow e1 e2 -> let r1 = eval s e1 in
                 let r2 = eval s e2 in
                 case (r1, r2) of
                     (Just v1, Just v2) -> Just ((**) v1 v2)
                     (_, _) -> Nothing