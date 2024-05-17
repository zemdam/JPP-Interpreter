module Main where
import System.Environment (getArgs)
import Bnfc.Par (myLexer, pProgram)
import System.IO (hPutStrLn, stderr)
import TypeChecker (typeChecker)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [p] -> readFile p >>= run
        [] -> getContents >>= run
        _ -> hPutStrLn stderr "too many arguments"

run :: String -> IO ()
run s = 
    case pProgram (myLexer s) of
        Left e -> hPutStrLn stderr ("parse error: " ++ e)
        Right p -> case typeChecker p of
            Left e -> hPutStrLn stderr ("type check error: " ++ e)
            Right _ -> return ()