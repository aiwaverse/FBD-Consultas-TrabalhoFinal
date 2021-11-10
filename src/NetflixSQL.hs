-- |
-- Copyright: (c) 2021 Agatha Lenz
-- SPDX-License-Identifier: MIT
-- Maintainer: Agatha Lenz <aiwavision@protonmail.com>
--
-- See README for more info
module NetflixSQL
  ( localConnection,
    searchQuery,
    runQuery,
  )
where

import Data.Text (pack, unpack)
import Database.HDBC
  ( IConnection (prepare),
    Statement (execute),
    fetchAllRows,
    getColumnNames,
    toSql,
  )
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Printer
import Queries
import Relude
import Relude.Extra.Map
import System.IO (hSetEcho)
import Text.PrettyPrint.Boxes (printBox)

localConnection :: Text -> Text -> Text -> IO Connection
localConnection dbname user password = connectPostgreSQL $ unpack conString
  where
    conString = "host=localhost dbname=" <> dbname <> " user=" <> user <> " password=" <> password

searchQuery :: Int -> Queries -> Maybe (String, Int)
searchQuery i (Queries q) = lookup i q

runQuery :: Connection -> String -> [String] -> IO ()
runQuery c search args = do
  st <- prepare c search
  _ <- execute st (map (toSql @String) args)
  colNames <- map pack <$> getColumnNames st
  res <- fetchAllRows st
  printBox $ prettyPrintQuery colNames res
  hSetEcho stdin False
  _ <- getLine
  hSetEcho stdin True
  pure ()
