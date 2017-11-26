module EnvInteractif where

import Parse
import Expression
import Data.Maybe

---------------------- Commande -------------------------------
store = initStore

isCommand xs =
    if head xs == ':' then True else False

processExpression line =
    let parsedLine = parseExpression line in
    eval store (fromJust parsedLine)
    
main = do
    putStr "> "
    line <- getLine
    if isCommand line == False
        then putStrLn (show (processExpression line))
        else putStrLn "0"
    main