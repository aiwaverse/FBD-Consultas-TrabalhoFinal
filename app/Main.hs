module Main (main) where

import           Database.HDBC            (IConnection (prepare),
                                           Statement (execute), fetchAllRows,
                                           fetchAllRowsMap, getColumnNames)
import           Database.HDBC.PostgreSQL
import           Printer                  (createBoxes)
import           QueriesSQL               (projectName)
import           Text.PrettyPrint.Boxes   (printBox)

something :: IO ()
something = do
              c <- connectPostgreSQL "host=localhost dbname=TBF user=maya password=sapphic"
              st <- prepare c "select * from conta"
              _ <- execute st []
              res <- fetchAllRowsMap st
              printBox $ createBoxes res


main :: IO ()
main = something
