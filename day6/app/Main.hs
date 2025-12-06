{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where
import Data.List (transpose)
--import Data.List.Split (splitWhen)


main :: IO ()
main = do
    part2

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

trimSpace :: String -> String
trimSpace = filter (/= ' ')


part1 :: IO ()
part1 = do
    --rows <- reverse . lines <$> readFile "example.txt"
    rows <- reverse . lines <$> readFile "puzzle_input.txt"

    let ops = filter (/= "") $ map trimSpace $ split ' ' $ head rows
    let nums = transpose $ map (filter (/= "") . map trimSpace) $ map (split ' ') $ drop 1 rows

    print $ sum $ map (uncurry operate) $ zip ops nums


operate :: String -> [String] -> Integer
operate "*" nums = product $ map read nums
operate "+" nums = sum $ map read nums
operate _   _    = undefined

-- --------------------------



part2 :: IO ()
part2 = do
    --rows <- filter ((/= "") . trimSpace) . transpose . lines <$> readFile "example.txt"
    rows <- filter ((/= "") . trimSpace) . transpose . lines <$> readFile "puzzle_input.txt"

    let parts = makeChunks rows
    let valsOps = map splitOps parts

    print $ sum $ map execute valsOps

  where

    execute :: ([String], Char) -> Integer
    execute (xs, '+') = sum $ map read xs
    execute (xs, '*') = product $ map read xs
    execute _         = -1



    hasOps :: String -> Bool
    hasOps ('*':_) = True
    hasOps ('+':_) = True
    hasOps _       = False

    hasOps' = hasOps . reverse

    makeChunks :: [String] -> [[String]]
    -- we always start with an operation column
    makeChunks (x:xs) = mkChunks xs [x] []
    makeChunks x      = [x]


    mkChunks :: [String] -> [String] -> [[String]] -> [[String]]
    mkChunks []     []  res             = res 
    mkChunks []     cur res             = cur:res 
    mkChunks [x]    cur res             = (x:cur):res
    
    -- start a new operation
    mkChunks (x:xs) cur res | hasOps' x = mkChunks xs [x] (cur:res)

    -- inside the current operation
    mkChunks (x:xs) cur res             = mkChunks xs (x:cur) res

    splitOps :: [String] -> ([String], Char)
    splitOps xs = (map (reverse . drop 1 . reverse) xs, last $ last xs)
