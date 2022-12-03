#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/3.txt"
    let all = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQSTUVWXZY"
    let rs = map (readRucksack . T.unpack) (T.splitOn (T.pack "\n") (T.pack contents))
    let gs = chunks 3 (map allItems rs)
    let is = map inBoth rs
    let ps = map (map priority . nub) is
    let ps' = map sum ps
    let p = sum ps'
    let bs = map (nub . foldr intersect all) gs
    let pbs = concatMap (map priority) bs
    print ("Sum of priority: " ++ show p)
    print ("Priority of badges " ++ show (sum pbs))

type Item = Char
type Compartment = [Item]
type Rucksack = (Compartment, Compartment)

allItems :: Rucksack -> [Item]
allItems = uncurry (++)

readRucksack :: String -> Rucksack
readRucksack s = let l = length s in
    splitAt (l `div` 2) s

inBoth :: Rucksack -> [Item]
inBoth (l, r) = l `intersect` r



chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n rs = take n rs : chunks n (drop n rs)


priority :: Item -> Int
priority 'a' = 1 
priority 'b' = 2 
priority 'c' = 3 
priority 'd' = 4 
priority 'e' = 5 
priority 'f' = 6 
priority 'g' = 7 
priority 'h' = 8 
priority 'i' = 9 
priority 'j' = 10 
priority 'k' = 11
priority 'l' = 12
priority 'm' = 13
priority 'n' = 14
priority 'o' = 15
priority 'p' = 16
priority 'q' = 17
priority 'r' = 18
priority 's' = 19
priority 't' = 20 
priority 'u' = 21 
priority 'v' = 22 
priority 'w' = 23
priority 'x' = 24 
priority 'y' = 25 
priority 'z' = 26 
priority 'A' = 27 
priority 'B' = 28
priority 'C' = 29
priority 'D' = 30
priority 'E' = 31 
priority 'F' = 32 
priority 'G' = 33 
priority 'H' = 34 
priority 'I' = 35 
priority 'J' = 36
priority 'K' = 37 
priority 'L' = 38 
priority 'M' = 39 
priority 'N' = 40 
priority 'O' = 41 
priority 'P' = 42 
priority 'Q' = 43
priority 'R' = 44
priority 'S' = 45
priority 'T' = 46 
priority 'U' = 47 
priority 'V' = 48
priority 'W' = 49
priority 'X' = 50
priority 'Y' = 51 
priority 'Z' = 52 
priority c = error ("undefined item priority: " ++ c:"")
