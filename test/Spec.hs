module Main (main) where

import QueriesSQL (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
