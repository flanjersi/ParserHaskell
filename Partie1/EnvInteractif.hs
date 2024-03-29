module Main where

import Data.List.Split
import Parse
import Expression
import Data.Maybe
import System.Exit

---------------------------------- COMMANDES ----------------------------------
type Handler = [String] -> Store -> IO Store

data  Command = Command {
    name :: String, -- Nom de la  commande
    description  :: String, -- Description  de la  commande
    run ::  Handler  -- Le code à executer
}

------------------------------------------------QUIT
quitCommand = Command {
    name = "q",
    description = "Quitte le programme",
    run = quitFct
}

quitFct args store = exitSuccess

------------------------------------------------HELP

helpCommand = Command {
    name = "h",
    description = "Affiche les commandes et leur description",
    run = helpFct
}

helpFct args store =
    printDescriptions commands >>
    return store

printDescriptions command = case command of
    [] -> putStr ""
    (x:xs) -> putStrLn (name x ++ ": " ++ description x) >> printDescriptions xs

------------------------------------------------STORE

storeCommand = Command {
    name = "store",
    description = "Affiche ce qui est dans le store",
    run = storeFct
}

storeFct args store =
    printStore store >>
    return store

printStore store = case store of
    [] -> putStr ""
    ((var, val):xs) -> putStrLn (var ++ " = " ++ (show val)) >> printStore xs

------------------------------------------------SET

setCommand = Command {
    name = "set",
    description = "Set la variable x à la valeur a",
    run = setFct
}

setFct args store =
    return (addToStore store (head args) (read (args !! 1) :: Float))

------------------------------------------------UNSET

unsetCommand = Command {
    name = "unset",
    description = "Enleve la variable x du store",
    run = unsetFct
}

unsetFct args store =
    return (removeFromStore store (head args))

-----------------------

commands :: [Command]
commands = [helpCommand, storeCommand, setCommand, unsetCommand, quitCommand]

isCommand :: [Char] -> Bool
isCommand [] = False
isCommand xs =
    if head xs == ':' then True else False

getCommand :: [Command] -> String -> Maybe Command
getCommand cs nameCommand = case cs of
    [] -> Nothing
    (x:xs) -> if name x == nameCommand then Just x else getCommand xs nameCommand

------------------------------------------------

processExpression :: String -> Store -> IO Store
processExpression line store =
    let parsedLine = parseExpression line in
    if parsedLine == Nothing
    then return store
    else
        let maybeEval = eval store (fromJust parsedLine) in
        if maybeEval == Nothing
        then return store
        else putStrLn (show (fromJust maybeEval)) >>
             return store

processCommand :: String -> Store -> IO Store
processCommand line s =
    let args = splitOn " " line in
    let c = getCommand commands (head args) in
    case c of
        Nothing -> return s
        Just comm -> run comm (tail args) s

executeLines :: [String] -> Store -> IO Store
executeLines lines s = case lines of
    [] -> return s
    [x] -> executeLine x s
    (x:xs) -> do
              store <- executeLine x s
              executeLines xs store

executeLine :: String -> Store -> IO Store
executeLine line s =
    if isCommand line == False
        then do processExpression line s
        else do processCommand (tail line) s

------------------------------------------------ MAIN

launchEnvInteractif s = do
    putStr "> "
    line <- getLine
    store <- executeLine line s
    launchEnvInteractif store
    
main = launchEnvInteractif []