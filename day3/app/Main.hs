{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where


main :: IO ()
main = do
    part2

joltage :: String -> String
joltage s =
        joltage' s
    where
        maxInner :: Char
        maxInner = foldl max '0' $ drop 1 $ reverse s

        joltage' :: String -> String
        joltage' (c:cs) | c == maxInner = joltage'' cs
        joltage' (_:cs)                 = joltage' cs
        joltage' []                     = undefined

        joltage'' r = [maxInner, foldl max '0' r]


part1 :: IO ()
part1 = do
    --input <- readFile "example.txt"
    input <- readFile "puzzle_input.txt"

    let vals = map (read . joltage) $ lines input :: [Integer]
    print $ sum vals



-- ---------------------------------------------------------------------------


maxLetter :: Int -> String -> Char
maxLetter n s = foldl max '0' $ take n s

dropTill :: (Eq a) => a -> [a] -> [a]
dropTill x = drop 1 . dropWhile (/= x)

buildJoltage :: Int -> String -> String
buildJoltage 1 s = [maxLetter (length s) s]
buildJoltage n s =
    let
        l = length s
        dgt = maxLetter (l - n + 1) s
        rst = dropTill dgt s
    in
        dgt : buildJoltage (n-1) rst


part2 :: IO ()
part2 = do
    --input <- readFile "example.txt"
    input <- readFile "puzzle_input.txt"

    let vals = map (read . buildJoltage 12) $ lines input :: [Integer]
    print $ sum vals