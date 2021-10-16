-- |
-- Copyright: (c) 2021 Agatha Lenz
-- SPDX-License-Identifier: MIT
-- Maintainer: Agatha Lenz <aiwavision@protonmail.com>
--
-- See README for more info
module QueriesSQL
  ( projectName
  , something
  )
where

import Database.HDBC
  ( IConnection (prepare)
  , Statement (execute)
  , fetchAllRows
  , getColumnNames
  )
import Database.HDBC.PostgreSQL ( connectPostgreSQL, Connection )
import Printer
import Relude
import Data.Text (pack)
import Text.PrettyPrint.Boxes ( printBox )

localConnection :: String -> String -> String -> IO Connection
localConnection dbname user password = connectPostgreSQL conString
  where conString = "host=localhost dbname=" ++ dbname ++ " user=" ++ user ++ " password=" ++ password

defaultConnection :: IO Connection
defaultConnection = localConnection "TBF" "maya" "sapphic"

something :: IO ()
something = do
  c <- defaultConnection
  st <- prepare c "select * from conta"
  _ <- execute st []
  colNames <- map pack <$> getColumnNames st
  res <- fetchAllRows st
  printBox $ prettyPrintQuery colNames res

projectName :: Text
projectName = "QueriesSQL"
