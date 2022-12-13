module Shared where

import qualified Data.Text as T

toLines :: String -> [String]
toLines s = filter (/= "") $ map T.unpack $ T.splitOn (T.pack "\n") (T.pack s)

indexedList :: [a] -> [(Int, a)]
indexedList = indexedList' 1 where
    indexedList' n (a:as) = (n, a) : indexedList' (succ n) as
    indexedList' _ [] = []
