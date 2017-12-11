module Main where

import Data.List.Split
import Data.Either
import Parse
import Expression
import System.Exit
import System.IO
import Control.Exception

---------------------------------- COMMANDES ----------------------------------
type Handler = [String] -> Store -> IO Store

data  Command = Command {
    name :: String, -- Nom de la  commande
    description  :: String, -- Description  de la  commande
    run ::  Main.Handler  -- Le code à executer
}

------------------------------------------------QUIT
quitCommand = Command {
    name = "q",
    description = "Quitte le programme. :q",
    run = quitFct
}

quitFct args store = exitSuccess

------------------------------------------------HELP

helpCommand = Command {
    name = "h",
    description = "Affiche les commandes et leur description. :h",
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
    description = "Affiche ce qui est dans le store. :store",
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
    description = "Set la variable x à la valeur a. :set x 1",
    run = setFct
}

setFct args store =
    return (addToStore store (head args) (read (args !! 1) :: Float))

------------------------------------------------UNSET

unsetCommand = Command {
    name = "unset",
    description = "Enleve la variable x du store. :unset x",
    run = unsetFct
}

unsetFct args store =
    return (removeFromStore store (head args))

------------------------------------------------UNSETALL

unsetAllCommand = Command {
    name = "unsetAll",
    description = "Vide le store. :unsetAll",
    run = unsetAllFct
}

unsetAllFct args store =
    return initStore

------------------------------------------------LOAD

loadCommand = Command {
    name = "l",
    description = "Execute les expressions et commandes d'un fichier. :l filename",
    run = loadFct
}
      
loadFct args store = do
    contents <- try (readFile (head args)) :: IO (Either IOException String)
    case contents of
        Left ex -> putStrLn "Ce fichier n'existe pas" >> return store
        Right text -> executeLines (lines text) store

------------------------------------------------INFO

infoCommand = Command {
    name = "info",
    description = "A partir d'une string, affiche son expression sans l'évaluer. Il ne doit pas y avoir d'espace dans l'expression. :info 1+1*3",
    run = infoFct
}

infoFct args store =
    let parsedLine = parseExpression (head args) in
    putStrLn (show parsedLine) >>
    return store

-----------------------

commands :: [Command]
commands = [helpCommand, storeCommand, setCommand, unsetCommand, unsetAllCommand, loadCommand, quitCommand, infoCommand]

isCommand :: [Char] -> Bool
isCommand [] = False
isCommand xs =
    if head xs == ':' then True else False

getCommand :: [Command] -> String -> Either String Command
getCommand cs nameCommand = case cs of
    [] -> Left (nameCommand ++ " n'est pas une commande. :h pour afficher la liste des commandes")
    (x:xs) -> if name x == nameCommand then Right x else getCommand xs nameCommand
    
------------------------------------------------

processExpression :: String -> Store -> IO Store
processExpression line store =
    let parsedLine = parseExpression line in
    case parsedLine of
        Left ex -> putStrLn ex >> return store
        Right expr -> let result = eval store expr in
                      case result of
                        Left ex -> putStrLn ex >> return store
                        Right r -> putStrLn (show r) >>
                                   return store

processCommand :: String -> Store -> IO Store
processCommand line s =
    let args = splitOn " " line in
    let c = getCommand commands (head args) in
    case c of
        Left ex -> putStrLn ex >> return s
        Right comm -> run comm (tail args) s
    
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