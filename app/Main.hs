module Main (main) where

import QueriesSQL (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
