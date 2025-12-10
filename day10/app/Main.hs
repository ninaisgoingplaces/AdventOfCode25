{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where


import Data.List (subsequences)
import Data.Foldable (foldl')



main :: IO ()
main = do
    part1

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


makeLights :: String -> [Int]
makeLights = map (\c -> if c == '.' then 0 else 1) . innerString

makeButtons :: String -> [Int]
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


parseLine :: (String, [String], String) -> ([Int], [[Int]], [Int])
parseLine (sLights, sButtons, joltages) =
    let lights = makeLights sLights
        buttons = map makeButtons sButtons
    in
        (lights, buttons, [])

addButtons :: [Int] -> [Int] -> [Int]
addButtons (x:xs) (y:ys) = (x + y):addButtons xs ys
addButtons (x:xs) []     = x:addButtons xs []
addButtons []     (y:ys) = y:addButtons [] ys
addButtons []     []     = []

removeTrailingZeros :: [Int] -> [Int]
removeTrailingZeros = reverse . dropWhile (==0) . reverse

solveLine :: [Int] -> [[Int]] -> [[[Int]]]
solveLine target buttons =
   let
        bs = subsequences buttons
        res = map (removeTrailingZeros . map (`mod` 2) . foldl' addButtons []) bs
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

    --let lights = map (\(x, _, _) -> x) rows'

    --lights' <- mapM lightsTarget lights

    --mapM_ printTarget lights'

    --print $ head $ map parseLine rows'

    -- print $ makeButtons "(0,3,4)"

    --print $ addButtons [0,1,1] [1,1,0,0,1]

    --let (target, buttons, _) = head $ map parseLine rows'
    --print "Target"
    --print target
    --print "Buttons"
    --print buttons
    --print $ map length $ solveLine target buttons

    print $ sum $ map (solve . parseLine) rows'

-- --------------------------

