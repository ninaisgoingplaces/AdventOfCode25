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


part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let y = fromJust $ elemIndex 'S' (head rows)

    print y

    print $ sum $ map snd $ withObsv 40 y rows




withObsv :: Int -> Int -> [String] -> [(Int, Int)]
withObsv n y0 rows = 
    let res   = map head $ qRun (take n rows) [[y0]]
        obsv  = counts res
        rows' = drop n rows
    in 
        if 
            rows' /= [] 
        then
            concatMap (\(y', prob) -> map (\(a,b) -> (a, b*prob)) $ withObsv n y' rows') obsv
        else 
            obsv


qStep :: Int -> String -> [Int]
qStep y rs | rs !! y == '^' = [y-1, y+1]
qStep y _                   = [y]

type TachPath = [Int]

qRun :: [String] -> [TachPath] -> [TachPath]
qRun []     ps = ps
qRun (r:rs) ps = do
    p  <- ps
    y' <- qStep (head p) r
    let ps' = y':p
    qRun rs [ps']

