{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where

import GHC.Utils.Misc (split)

main :: IO ()
main = do
    part2


to2Tuple :: [a] -> (a, a)
to2Tuple [a, b] = (a, b)
to2Tuple _      = undefined


isInvalid :: Integer -> Bool
isInvalid n =
    let s = show n :: String
        l = length s
        l2 = l `div` 2
    in
        if l `mod` 2 == 1
        then
            False
        else
            take l2 s == drop l2 s

invalidCodes :: Integer -> Integer -> [Integer]
invalidCodes a b =
    filter isInvalid [a..b]


part1 :: IO ()
part1 = do
    --input <- readFile "example.txt"
    input <- readFile "puzzle_input.txt"
    --let parts = map ((read :: String -> Integer) . split '-') (split ',' input)
    let parts = map (split '-') (split ',' input)
    let ns = map (to2Tuple . map read) parts :: [(Integer, Integer)]
    let diffs = map (uncurry (flip (-))) ns
    print parts
    print ns
    print diffs

    let ics = map (uncurry invalidCodes) ns

    --print ics

    print $ sum $ map sum ics


-- ---------------------------------------------------------------------------

isInvalid2 :: Integer -> Bool
isInvalid2 n =
    let s = show n :: String
        l = length s
        l2 = l `div` 2
        chunkSizes = filter (\m -> (l `mod` m) == 0) [1..l2] -- ways s is divisable without rest
    in
        any (testChunks s) chunkSizes

    where
        testChunks :: String -> Int -> Bool
        testChunks s m =
                testChunks' $ drop m s
            where
                tmpl = take m s
                testChunks' :: String -> Bool
                testChunks' [] = True
                testChunks' r' = if take m r' == tmpl then testChunks' (drop m r') else False



invalidCodes2 :: Integer -> Integer -> [Integer]
invalidCodes2 a b =
    filter isInvalid2 [a..b]


part2 :: IO ()
part2 = do
    --input <- readFile "example.txt"
    input <- readFile "puzzle_input.txt"
    let parts = map (split '-') (split ',' input)
    let ns = map (to2Tuple . map read) parts :: [(Integer, Integer)]

    let ics = map (uncurry invalidCodes2) ns

    --print ics

    print $ sum $ map sum ics

