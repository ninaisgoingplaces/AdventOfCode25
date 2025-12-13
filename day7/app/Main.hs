{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use list literal pattern" #-}
module Main where
import Data.Set (fromList, toList, member, difference, size, intersection)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Test.LeanCheck.Stats (counts)

main :: IO ()
main = do
    part2

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)



part1 :: IO ()
part1 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"


    let rows' = reverse $ run rows []

    let splits = map (toList . uncurry intersection . both fromList) $ zip (map whereBeams rows') (map whereSplitter (drop 1 rows'))

    print $ sum $ map length splits



countSplits :: [String] -> [Int]
countSplits (x0:x1:xs) =
    let
        b0 = fromList $ whereBeams x0
        b1 = fromList $ whereBeams x1
        n = max (size (b1 `difference` b0) - 1) 0
    in
        n:countSplits (x1:xs)
countSplits (_:[]) = []
countSplits [] = []


run :: [String] -> [String] -> [String]
run (x0:x1:xs) acc =
    let
        x1' = step x0 x1
    in
        run (x1':xs) (x0:acc)

run (x:[]) acc = x:acc
run [] acc     = acc


step :: String -> String -> String
step cs ns =
    let newBeams = fromList $ concatMap (`doSplit` ns) $ whereBeams cs
    in
        map (\(i, x) -> if i `member` newBeams then '|' else x) $ zip [0..] ns

whereBeams :: String -> [Int]
whereBeams = whereThing '|'

whereSplitter :: String -> [Int]
whereSplitter = whereThing '^'


whereThing :: Char -> String -> [Int]
whereThing c = wb 0 []
    where
        wb :: Int -> [Int] -> String -> [Int]
        wb _ acc [] = acc
        wb n acc (x:xs) | x == c  = wb (n+1) (n:acc) xs
        wb n acc ('S':xs)         = wb (n+1) (n:acc) xs
        wb n acc (_:xs)           = wb (n+1) acc xs


doSplit :: Int -> String -> [Int]
doSplit _ [] = []
doSplit n xs = if (xs !! n) == '^' then [n-1, n+1] else [n]


-- --------------------------

type Position = Int
type Path = [Position]
type Row = String
type TimeInterval = Int
type ProbFrequency = Int

part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let y = fromJust $ elemIndex 'S' (head rows)

    print y

    print $ sum $ map snd $ withObsv 40 y rows


{-
    The particle starts at a vertical *Position* and runs qm-style through
    the *[Row]*s of the apparatus. It is observed every *TimeInterval*
    collapsing the wavefunction into a collection *[(ProbFrequency, Position)]* of
    *Position*s with a *ProbFrequency*.

    The trick is, that every n Steps we completely forget about all the
    multiple paths, that broughts us to a given y-Position and just
    start the next part of the journey from y, but scaling the weight
    of this y-Position with the number of paths, that ended in this position.
    This way forgetting/weighting (=observing) is a way (nature's way?)
    to deal with the time/space complexity of the problem.
-}
withObsv :: TimeInterval -> Position -> [Row] -> [(ProbFrequency, Position)]
withObsv dt y0 rows = 
    let res   = map head $ qRun y0 (take dt rows) -- only interested in the end-position of all paths
        obsv  = counts res      -- counts :: Eq a => [a] -> [(a, Int)]
        rows' = drop dt rows
    in 
        if 
            rows' /= [] 
        then
            concatMap (\(y', prob) -> map (\(a,b) -> (a, b*prob)) $ withObsv dt y' rows') obsv
        else 
            obsv

{-
    A particle travels one *Row* downwards starting at vertical *Position*.
    As this is quantum mechanical this could lead to several timelines
    given by *[Position]*
-}
qStep :: Position -> Row -> [Position]
qStep y rs | rs !! y == '^' = [y-1, y+1]
qStep y _                   = [y]


{-
    A complete quantum mechanical run of the next *[Row]*s of the apparatus
    starting from *Position* y. We will return all possible paths
    as *[[Position]]*.
-}
qRun :: Position -> [Row] -> [[Position]]
qRun y []     = [[y]]
qRun y (r:rs) = do
    y' <- qStep y r
    p  <- qRun y' rs
    -- it would be more efficient to just return the endpoint
    -- as this is only used in withObsv, but I wanted to make clear
    -- what is happening physically here. And I don't think, that
    -- it's such a large time waste, because the paths are not too large.
    return $ p ++ [y']

