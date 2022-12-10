module Shared where

import qualified Data.Text as T

toLines :: String -> [String]
toLines s = filter (/= "") $ map T.unpack $ T.splitOn (T.pack "\n") (T.pack s)
