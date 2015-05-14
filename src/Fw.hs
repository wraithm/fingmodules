module Main where

import           Control.Arrow          (second)

import           Text.PrettyPrint       (render)

import           Language.Lambda.Eval
import           Language.Lambda.Pretty as L

import           Language.Fw.AST
import           Language.Fw.TypeCheck
import           Language.Fw.Erase
import           Language.Fw.Parser
import           Language.Fw.Pretty     as F

addContext :: Context Name -> [(Name, Exp)] -> Context Name
addContext _ [] = Empty
addContext ctx ((l,e):xs) = t (addContext (t ctx) xs)
  where t = TyBind l (typeOf ctx e)

main :: IO ()
main = do
    result <- parseProgFromFile "test.f"
    let elabResult = map (second erase) result
        Just mainProg = lookup "main" elabResult
        evalResult = eval elabResult mainProg
        ctx = addContext Empty result

    titled "1. Parsing" $
        mapM_ (\(name, e) -> putStr (name ++ " = ") >> print e) result

    titled "2. Pretty print" $
        putStrLn $ F.prettyProgram result

    titled "3. Type erasure" $
        putStrLn $ L.prettyProgram elabResult

    {-
    titled "TEST one" $
        let Just s = lookup "succ" result 
            Just z = lookup "zero" result
            Just one = lookup "one" result 
            tsucc = typeOf Empty s 
            tz = typeOf Empty z
            env = TyBind "zero" tz $ TyBind "succ" tsucc Empty 
        in do
            print one
            prettyPrint (F.prettyExp one)
            prettyPrint (prettyType (typeOf env one))
    -}

    titled "4. Type check" $
        putStrLn $ prettyContext ctx

    putStrLn "5. Evaluation"
    prettyPrint $ L.prettyExp evalResult
  where
    newline = putStr "\n"
    prettyPrint = putStrLn . render

    titled t f = putStrLn t >> f >> newline
