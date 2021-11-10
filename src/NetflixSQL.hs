-- |
-- Copyright: (c) 2021 Agatha Lenz
-- SPDX-License-Identifier: MIT
-- Maintainer: Agatha Lenz <aiwavision@protonmail.com>
--
-- See README for more info
module NetflixSQL
  ( projectName
  , something
  )
where

import Database.HDBC
  ( IConnection (prepare)
  , Statement (execute)
  , fetchAllRows
  , getColumnNames, toSql
  )
import Database.HDBC.PostgreSQL ( connectPostgreSQL, Connection )
import Printer
import Relude
import Data.Text (pack, unpack)
import Text.PrettyPrint.Boxes ( printBox )
import Queries

localConnection :: Text -> Text -> Text -> IO Connection
localConnection dbname user password = connectPostgreSQL $ unpack conString
  where conString = "host=localhost dbname=" <> dbname <> " user=" <> user <> " password=" <> password

defaultConnection :: IO Connection
defaultConnection = localConnection "Netflix" "maya" "sapphic"

something :: IO ()
something = do
  c <- defaultConnection
  st <- prepare c consultaSerie
  _ <- execute st [toSql @String "Bob Esponja"]
  colNames <- map pack <$> getColumnNames st
  res <- fetchAllRows st
  printBox $ prettyPrintQuery colNames res

projectName :: Text
projectName = "QueriesSQL"
