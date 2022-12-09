#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import qualified Data.Text as T
import Data.List

main :: IO ()
main = do
    contents <- readFile "inputs/9.txt"
    let ms = filter (/= "") $ map T.unpack (T.splitOn (T.pack "\n") (T.pack contents))
    let h = (0,0)
    let t = Tail (0,0) []
    let (h', Tail t' ts) = foldl moveHead (h, t) (map readMove ms)
    -- 239 is too low
    -- 6265 is too low
    print $ length $ nub (t' : ts)

data Dir = Up | L | R | Down deriving (Show)
type Move = (Dir, Int)

type Head = (Int, Int)
data Tail = Tail (Int,Int) [(Int, Int)] deriving (Show)

readMove :: String -> Move
readMove ('U' : s) = (Up, read s)
readMove ('L' : s) = (L, read s)
readMove ('R' : s) = (R, read s)
readMove ('D' : s) = (Down, read s)

moveHead :: (Head,Tail) -> Move -> (Head, Tail)
moveHead (h,t) (_, 0) = (h, t)
moveHead (h,t) (Up, n) = moveHead (h', moveTail h h' t) (Up, n-1)
    where h' = (fst h, snd h - 1)
moveHead (h, t) (Down, n) = moveHead (h', moveTail h h' t) (Down, n-1)
    where h' = (fst h, snd h + 1)
moveHead (h, t) (L, n) = moveHead (h', moveTail h h' t) (L, n-1)
    where h' = (fst h - 1, snd h)
moveHead (h, t) (R, n) = moveHead (h', moveTail h h' t) (R, n-1)
    where h' = (fst h + 1, snd h)


moveTail :: 
    Head {- current head position -}
    -> Head {- new head position -}
    -> Tail {- current tail position -}
    -> Tail {- new tail position -}
moveTail h h' (Tail t ts) = if distance h' t > 1 then Tail h (t: ts) else Tail t ts

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x, y) (x', y') = max (abs (x' - x)) (abs (y' - y))
