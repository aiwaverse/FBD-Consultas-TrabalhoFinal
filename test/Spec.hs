module Main (main) where

import QueriesSQL (projectName)

import Relude

main :: IO ()
main = putTextLn ("Tests for " <> projectName)
