module Main where

import           Text.PrettyPrint       (render)

import           Language.Lambda.Eval
import           Language.Lambda.Parser
import           Language.Lambda.Pretty as L

main :: IO ()
main = do
    result <- parseProgFromFile "test.l"
    let Just mainProg = lookup "main" result
        evalResult = eval result mainProg

    mapM_ (\(name, e) -> putStr (name ++ " = ") >> print e) result

    newline

    putStrLn $ L.prettyProgram result

    newline

    print evalResult
    putStrLn $ render $ L.prettyExp evalResult
  where
    newline = putStr "\n"
