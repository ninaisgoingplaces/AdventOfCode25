{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where


import Data.Map.Strict (Map, fromList, (!), lookup, insert, empty)
import Control.Monad (guard)
import Prelude hiding (lookup)



--import qualified Data.Set 



main :: IO ()
main = do
    part2

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l


innerString :: String -> String
innerString s = take (length s - 2) $ drop 1 s

keyVal :: String -> (String, [String])
keyVal s =
    let parts = split ' ' s
        key = take 3 $ head parts
        vals = drop 1 parts
    in
        (key, vals)

getPaths :: String -> Map String [String] -> String -> [String] -> [[String]]
getPaths end _  dv path | dv == end = [path]
getPaths end mp dv path             =
    let
        outs = mp ! dv
    in do dv' <- outs
          guard (dv' `notElem` path)
          let path' = dv':path
          getPaths end mp dv' path'




part1 :: IO ()
part1 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let kvs = map keyVal rows

    print kvs
    let mp = fromList kvs

    print mp

    let paths = map reverse $ getPaths "out" mp "you" []

    print $ length paths

    --print rows


-- --------------------------


type Outlets = [String]
type OutlMap = Map String [String]
type Start = String
type Goal = String
type Path = [String]
type PathLookup = Map (Start, Goal) [Path]


gPs :: OutlMap -> PathLookup -> Outlets -> Goal -> (PathLookup, [Path])
gPs _  plu []       _   = (plu, [])
gPs _  plu (ol:_)   end | ol == end = (plu, [[end]])
gPs mp plu (ol:ols) end = 
    let (plu''', ps'') = case lookup (ol, end) plu of 
                            Just ps -> (plu, ps)
                            Nothing -> 
                                let outs = mp ! ol
                                    (plu', ps) = gPs mp plu outs end
                                    ps' = map (\p -> ol:p) ps
                                    plu'' = insert (ol, end) ps' plu' 
                                in
                                    (plu'', ps')
        (plu'''', ps''') = gPs mp plu''' ols end
    in
        (plu'''', ps'' ++ ps''')


part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example2.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let kvs = ("out", []):map keyVal rows
    --print kvs
    let mp = fromList kvs

    let (_, ps1) = gPs mp empty ["svr"] "fft"
    let n1 = length ps1
    print n1

    let (_, ps2) = gPs mp empty ["fft"] "dac"
    let n2 = length ps2
    print n2

    let (_, ps3) = gPs mp empty ["dac"] "out"
    let n3 = length ps3
    print n3

    print $ n1 * n2 * n3



    putStrLn "ready"