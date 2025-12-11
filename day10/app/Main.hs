{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where


import Data.List (subsequences, elemIndex)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
--import Control.Monad (guard)
import Data.Set (fromList, toList, difference)
--import Control.Monad (forM)


main :: IO ()
main = do
    part2

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l


parts :: [a] -> (a, [a], a)
parts xs =
    let l = length xs
    in
        (head xs, take (l-2) $ drop 1 xs, last xs)


innerString :: String -> String
innerString s = take (length s - 2) $ drop 1 s

type Lights = [Int]

makeLights :: String -> Lights
makeLights = map (\c -> if c == '.' then 0 else 1) . innerString

type Buttons = [Int]

makeButtons :: String -> Buttons
makeButtons s =
    let s' = innerString s
        vals = map read $ split ',' s' :: [Int]
    in
        reverse $ run vals 0 []

  where
    run :: [Int] -> Int -> [Int] -> [Int]
    run (n:ns) i acc | n == i = run ns (i+1) (1:acc)
    run (n:ns) i acc          = run (n:ns) (i+1) (0:acc)
    run []     _ acc          = acc

type Joltages = [Int]

makeJoltage :: String -> Joltages
makeJoltage sJt = map read . split ',' $ innerString sJt


parseLine :: (String, [String], String) -> (Lights, [Buttons], Joltages)
parseLine (sLights, sButtons, sJoltages) =
    let lights = makeLights sLights
        buttons = map makeButtons sButtons
        maxButton = maximum $ map length buttons
        safety = replicate maxButton 0 :: [Int]
        buttons' = map (++ safety) buttons
        joltages = makeJoltage sJoltages
    in
        (lights, buttons', joltages)


addButtons :: Buttons -> Buttons -> Buttons
addButtons (x:xs) (y:ys) = (x + y):addButtons xs ys
addButtons (x:xs) []     = x:addButtons xs []
addButtons []     (y:ys) = y:addButtons [] ys
addButtons []     []     = []

addAllButtons :: [Buttons] -> Buttons
addAllButtons = foldl' addButtons []

removeTrailingZeros :: [Int] -> [Int]
removeTrailingZeros = reverse . dropWhile (==0) . reverse

solveLine :: [Int] -> [[Int]] -> [[[Int]]]
solveLine target buttons =
   let
        bs = subsequences buttons
        res = map (removeTrailingZeros . map (`mod` 2) . addAllButtons) bs
        target' = removeTrailingZeros target
        solutions = map snd $ filter ((==target') . fst) $ zip res bs
    in
        solutions

solve :: ([Int], [[Int]], [Int]) -> Int
solve (target, buttons, _) = minimum $ map length $ solveLine target buttons

part1 :: IO ()
part1 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"
    let rows' = map (parts . split ' ') rows


    print $ sum $ map (solve . parseLine) rows'


-- --------------------------

minimumAt :: [Int] -> (Int, Int)
minimumAt xs =
    let x0 = minimum $ filter (/= 0) xs
    in (fromJust $ elemIndex x0 xs, x0)

maximumAt :: [Int] -> (Int, Int)
maximumAt xs =
    let x0 = maximum $ filter (/= 0) xs
    in (fromJust $ elemIndex x0 xs, x0)

part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"
    let rows' = map (parts . split ' ') rows

 


    let results = do row <- take 3 rows'
                     let (_, buttons, joltages) = parseLine row
                     return $ getResult joltages buttons

    print results
    print $ sum results



multButton :: Int -> Buttons -> Buttons
multButton n = map (*n)

getResult :: Joltages -> [Buttons] -> Int
getResult joltages bs =
    let
        res = run2 joltages bs
    in
        if not (null res) then minimum $ map sum res else -1



minimum0 :: [Int] -> Int
minimum0 [] = 0
minimum0 xs =
    let xs' = filter (/=0) xs
    in if null xs' then 0 else minimum xs'

step :: Joltages -> [Buttons] -> (Int, [Joltages], [Buttons])
step joltages bs =
    let
        bSum = addAllButtons bs
        (i, _) = minimumAt bSum
        jMax = joltages !! i

        --(i, jMin) = minimumAt joltages
        rlvButtons = filter (\b -> b !! i == 1) bs
        presses = possis jMax rlvButtons

        possibilities = map (\p -> map (uncurry multButton) $ zip p rlvButtons ) presses
        combined = map ((\b -> map ((-1) *) b) . addAllButtons) possibilities
        posJoltages = (toList . fromList) $ filter (all (>=0)) $ map (addButtons joltages) combined

        bs' = toList $ difference (fromList bs) (fromList rlvButtons)
    in
        (jMax, posJoltages, bs')

possis :: Int -> [a] -> [[Int]]
possis n [_]    = [[n]]
possis n (_:xs) = do
        j <- [0..n]
        js <- possis (n-j) xs
        return $ j:js
possis _ []     = []



run2 ::  Joltages -> [Buttons] -> [[Int]]
run2 []       _  = [[]]
run2 joltages _  | all (==0) joltages = [[]]
run2 joltages bs =
    let
        (jMin', posJoltages', bs') = step joltages bs
        res = do {
            joltages' <- posJoltages';
            path <- run2 joltages' bs';
            return $ jMin':path
        }
    in
        res
