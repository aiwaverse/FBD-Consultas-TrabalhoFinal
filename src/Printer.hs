{-# LANGUAGE TypeApplications #-}

module Printer (createBoxes, getLargestStringSize) where

import qualified Data.Map.Strict as Map
import Database.HDBC (SqlValue, fromSql)
import Relude
import Text.PrettyPrint.Boxes (Box, nullBox)
import Data.Map.Strict (findWithDefault)

getLargestStringSize :: [String] -> Int
getLargestStringSize xs = go xs 0
  where
    go [] acc = acc
    go (y : ys) acc = if length y > acc then go ys (length y) else go ys acc

getAllValues :: [Map.Map String String] -> String  -> [String]
getAllValues m k = map (findWithDefault "" k) m


createBoxes :: [Map.Map String SqlValue] -> Box
createBoxes values = case keys of
  Nothing -> nullBox
  Just kys -> let vals = map (\k -> Map.insert k (getAllValues sqlNamesString k) Map.empty) kys in undefined
  where
    -- converts sqlValues to Strings
    sqlNamesString = map (Map.map (\v -> fromSql @String v ++ " ")) values
    -- all the keys, I think it is never nothing
    keys = viaNonEmpty (Map.keys . head) sqlNamesString
    elems = map Map.elems sqlNamesString
