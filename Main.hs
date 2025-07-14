module Main where

import Pretty

main :: IO ()
main = putStrLn $ render $ pretty abcd


data SExpr = SExpr [SExpr] | Atom String deriving Show

pretty :: SExpr -> Doc ()
pretty (Atom s) = text s
pretty (SExpr xs) = (text "(") <> ((sep $ map pretty xs) <> text ")")

abcd = SExpr [Atom "a",Atom "b"]

main2= putStrLn $ renderWith defaultOptions { optsPageWidth = 15 } $ (pretty abcd `fuck` (pretty abcd `fuck` pretty abcd) `fuck` pretty abcd)