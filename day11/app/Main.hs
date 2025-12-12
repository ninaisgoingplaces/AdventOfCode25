{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where


import Data.Map.Strict (Map, fromList, (!))
import Control.Monad (guard)
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

part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example2.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let kvs = ("out", []):map keyVal rows
    --print kvs
    let mp = fromList kvs

    -- -- first all beginning paths
    -- let toSvr = mp ! "svr"
    -- print toSvr
    -- let startFFT = concatMap (\dst -> getPaths dst mp "fft" []) toSvr
    -- print startFFT

    --let fft2dac = getPaths "dac" mp "fft" []
    --print $ length fft2dac

    -- print $ length $ getPaths "fft" mp "dac" []
    -- print $ length $ getPaths "dac" mp "fft" []

    --print $ length $ getPaths "dac" mp "svr" ["fft"]

    -- let svr2dac2fft2out = do p1 <- getPaths "dac" mp "svr" ["fft"]
    --                          let p1' = take (length p1 - 1) p1
    --                          --  p2 <- getPaths "fft" mp "dac" p1'
    --                          --  getPaths "out" mp "fft" p2
    --                          getPaths "fft" mp "dac" p1'
    --                          --getPaths "out" mp "fft" p2

    -- print $ length svr2dac2fft2out

    -- we have to go through fft first?

    let svr2fft2dac2out = do p1 <- getPaths "fft" mp "svr" []
                             p2 <- getPaths "dac" mp "fft" p1                            
                             getPaths "out" mp "dac" p2

    print $ length svr2fft2dac2out



    --print $ length svr2dac2fft2out + length svr2fft2dac2out

    -- let toFFT = getPaths "fft" mp "svr" ["dac"]
    -- print toFFT

    -- let fromFttToDAC = concatMap (getPaths "dac" mp "fft") toFFT
    -- print fromFttToDAC

    --print mp

    -- richtig:
    -- let paths = getPaths "out" mp "svr" []
    -- let paths' = filter (\p -> "fft" `elem` p && "dac" `elem` p) paths
    -- print $ length paths'

    -- let otherPaths = getPaths "out" mp "dac" []
    -- print $ length otherPaths
    -- print $ length $ Data.Set.fromList $ map Data.Set.fromList otherPaths

    --let otherPaths' = getPaths "dac" mp "svr" []
    --print $ length otherPaths'

    --print rows

    putStrLn "ready"