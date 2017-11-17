module Expression where

{-
1. Doit on mettre dans des modules différents ?
2. Est ce qu'on doit changer la grammaire ? pour éviter d'avoir des parenthèse obligatoire pour le parseur
3. Doit on mettre les déclarations dans un fichier à part ?
-}
------------------------ Expr ---------------------------

data Expr = Const Float
    | Variable String
    | Uni Op1 Expr
    | Bin Op2 Expr Expr
    deriving Show

data Op1 = Minus deriving Show
data Op2 = Add | Mult | Pow deriving Show

getOperator1 :: Num a => Op1 -> (a -> a)
getOperator1 op = case op of
    Minus -> negate
    
getOperator2 :: Floating a => Op2 -> (a -> a -> a)
getOperator2 op = case op of
    Add -> (+)
    Mult -> (*)
    Pow -> (**)

------------------------ Store --------------------------------

type Store = [(String, Float)]

initStore :: Store
initStore = []

addToStore :: Store -> String -> Float -> Store
addToStore store var val = (var, val):store

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
               let op = getOperator1 o in
               case r of
                    Nothing -> Nothing
                    Just v -> Just (op v)
    Bin o e1 e2 -> let r1 = eval s e1 in
                   let r2 = eval s e2 in
                   let op = getOperator2 o in
                   case (r1, r2) of
                       (Just v1, Just v2) -> Just (op v1 v2)
                       (_, _) -> Nothing