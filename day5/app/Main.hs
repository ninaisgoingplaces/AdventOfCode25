{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where
import Data.Semigroup (diff)



main :: IO ()
main = do
    part2

split :: Eq a => a -> [a] -> ([a], [a])
split x xs = (takeWhile (/= x) xs, drop 1 $ dropWhile (/= x) xs)


both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

shesInside :: Int -> (Int, Int) -> Bool
shesInside x (x0, x1) = (x >= x0) && (x <= x1)

part1 :: IO ()
part1 = do
    -- rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let (freshRanges', availIngr') = split "" rows
    let freshRanges = map (both read . split '-') freshRanges' :: [(Int, Int)]
    let availIngr = map read availIngr' :: [Int]

    --print freshRanges
    --print availIngr

    let diffs = zip (map ( uncurry (flip (-))) freshRanges) freshRanges
    print diffs
    print (maximum diffs)
    print (minimum diffs)

    let result = map (\x -> any (shesInside x) freshRanges) availIngr

    print $ length $ filter id result


-- --------------------------

part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let (freshRanges', availIngr') = split "" rows
    let freshRanges = map (both read . split '-') freshRanges' :: [(Int, Int)]

    print $ adjScndRange (freshRanges !! 2) (freshRanges !! 3)
    print $ adjScndRange (freshRanges !! 3) (freshRanges !! 2)

    let res = foldl (\rs r -> r : concatMap (adjScndRange r) rs)  [head freshRanges] (drop 1 freshRanges)

    let diffs = map ((+1) . uncurry (flip (-))) res

    --print diffs
    print $ sum diffs


-- adjust the second range with respect to the first
adjScndRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
adjScndRange x y =
        removeInvalidRanges $ combineRs x y

    where
        -- y completely inside x
        combineRs x@(x0, x1) y@(y0, y1) | y0 >= x0 && y1 <= x1 = []

        -- just lower bound of y in x
        combineRs x@(x0, x1) y@(y0, y1) | x0 <= y0 && y0 <= x1 = [(x1+1, y1)]

        -- just upper bound of y in x
        combineRs x@(x0, x1) y@(y0, y1) | x0 <= y1 && y1 <= x1 = [(y0, x0-1)]

        -- x completely inside y
        combineRs x@(x0, x1) y@(y0, y1) | x0 >= y0 && x1 <= y1 = [(y0, x0-1), (x1+1, y1)]

        -- otherwise there is no overlap
        combineRs _ y = [y]

        removeInvalidRanges :: [(Int, Int)] -> [(Int, Int)]
        removeInvalidRanges = filter (uncurry (<=))