import System.Environment
import System.Exit
import System.IO

generateTable count sides = 0

parseExpression args = putStrLn "Not done yet."

parse args
    | null args || elem (head args) ["-h", "--help"] = printUsage >> exitWithSuccess
    | elem (head args) ["-v", "--version"] = printVersion >> exitWithSuccess
    | otherwise = parseExpression args 

printVersion = putStrLn "Dice Statistics v1.0"
printUsage = putStrLn "Usage: dicestats xdy [<|>|=] z"

exitWithSuccess = exitWith ExitSuccess

main = do
    args <- getArgs
    parse args
