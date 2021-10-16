module Printer where

import Relude hiding ((<>))
import Database.HDBC

import Text.PrettyPrint.Boxes
import Data.Text as T(length, unpack)

-- | Utility function since lists of lists are very common
-- Since this only uses fmap, it can technically work with any double-functor case
-- >>> doubleMap (+3) [[1,2],[3,4]]
-- [[4,5],[6,7]]
doubleMap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
doubleMap f = fmap (fmap f)

-- infix priority for <$$>
-- same as <$>
infixl 4 <$$>
-- | Operator for doubleMap
-- >>> (+3) <$$> [[1,2],[3,4]]
-- [[4,5],[6,7]]
-- >>> (+3) <$$> Just (Just 3)
-- Just (Just 6)
(<$$>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
(<$$>) = doubleMap

-- | Given the column names and the values of a query, generates a pretty box with the info
prettyPrintQuery :: [Text] -> [[SqlValue]] -> Box
prettyPrintQuery colNames colValues = titleBox /+/ foldl' (//) nullBox listOfBoxes
  where
    valRows = fromSql @Text <$$> colValues
    allRows = colNames : valRows
    widths = getLargestWidths allRows
    titleBox = foldl' (<>) nullBox $ map (foldl' (<+>) nullBox) $ generateBoxesAlligned [colNames] widths
    listOfBoxes = map (foldl' (<+>) nullBox) $ generateBoxesAlligned valRows widths

-- | Generates a list of list of boxes using the specified widths
-- the first width is used on all the first values, and so on
generateBoxesAlligned :: [[Text]] -> [Int] -> [[Box]]
generateBoxesAlligned content sizes = map (zipWith createBox sizes) content
  where createBox sz t = alignHoriz right sz (text $ T.unpack t)

-- | Gets the largest widths of each column of a list of lists
-- >>> getLargestWidths (["pear", "banana"] :| [["apple", "lemon"], ["lime", "avocado"]])
-- [5,7]
getLargestWidths :: [[Text]] -> [Int]
getLargestWidths [] = []
getLargestWidths (names : rest) = go rest initialSizes
  where
    initialSizes :: [Int]
    initialSizes = fmap T.length names
    go :: [[Text]] -> [Int] -> [Int]
    go [] sizes = sizes
    go (c : cs) sizes = let colSizes = map T.length c in
                    go cs $ zipWith (\str sz -> if str > sz then str else sz) colSizes sizes
