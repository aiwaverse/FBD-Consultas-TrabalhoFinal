module Main (main) where

import QueriesSQL (projectName)
import Database.HDBC
import Database.HDBC.PostgreSQL

something :: IO ()
something = do 
              c <- connectPostgreSQL "host=localhost dbname=TBF user=maya password=sapphic"
              res <- quickQuery c "select * from conta" []
              print res


main :: IO ()
main = something
