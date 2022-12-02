#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/1.txt"
    let elves = readElves contents
    let calories = sortBy (flip compare) (map sum elves)
    print ("Maximum: " ++ show (maximum calories))
    print ("Sum of top three: " ++ show (sum (take 3 calories)))

type Elf = [Int]

readElf :: T.Text -> Elf
readElf e = map readInt (T.splitOn (T.pack "\n") e)

readElves :: String -> [Elf]
readElves s = map readElf (T.splitOn (T.pack "\n\n") (T.pack s))

readInt :: T.Text -> Int
readInt x = read ("0" ++ T.unpack x)
