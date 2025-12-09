{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where




main :: IO ()
main = do
    part2

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l

combinations :: [a] -> [(a, a)] -> [(a, a)]
combinations []     acc = acc
combinations (x:xs) acc = combinations xs (acc ++ map (\y -> (x, y)) xs)

rectangle :: ([Int], [Int]) -> Int
rectangle ([x0, y0],[x1, y1]) =
        let dx = abs (x0 - x1)
            dy = abs (y0 - y1)
        in
            (dx + 1) * (dy + 1)
rectangle _                   = undefined


part1 :: IO ()
part1 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let posis = map (map read . split ',') rows :: [[Int]]

    let res = map rectangle $ combinations posis []

    -- print posis
    print $ maximum res


-- --------------------------

type Point = (Int, Int)

makePairs :: [a] -> (a, a)
makePairs [x, y] = (x, y)
makePairs _      = undefined

-- corners :: Point -> Point -> [Point]
-- corners (x0, y0) (x1, y1) = [(x0, y0), (x1, y0), (x1, y1), (x0, y0)]

type Line = (Point, Point)

pointsOfLine :: Line -> [Point]
pointsOfLine ((x0, y0), (x1, y1)) | x0 < x1 && y0 == y1 = map (\x -> (x, y0)) [x0..x1]
pointsOfLine ((x0, y0), (x1, y1)) | x0 > x1 && y0 == y1 = reverse $ pointsOfLine ((x1, y1), (x0, y0))
pointsOfLine ((x0, y0), (x1, y1)) | x0 == x1 && y0 < y1 = map (\y -> (x0, y)) [y0..y1]
pointsOfLine ((x0, y0), (x1, y1)) | x0 == x1 && y0 > y1 = reverse $ pointsOfLine ((x1, y1), (x0, y0))
pointsOfLine (p, _)                                     = [p]

type Rectangle = (Point, Point)

-- between :: Int -> Int -> Int -> Bool
-- between a b x = 

-- Point in the inner rectangle; *not* on the border
pointInRectangle :: Rectangle -> Point -> Bool
pointInRectangle ((x0, y0), (x1, y1)) (x, y) =
    let minX = min x0 x1
        maxX = max x0 x1
        minY = min y0 y1
        maxY = max y0 y1
    in
        ((minX < x) && (x < maxX)) && ((minY < y) && (y < maxY))



part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let posis' = map (map read . split ',') rows
    let _ = posis' :: [[Int]]
    let posis = map makePairs posis'

    let xs = map fst posis
    let ys = map snd posis

    let x_0 = minimum xs - 1
    let x_1 = maximum xs + 1
    let y_0 = minimum ys - 1
    let y_1 = maximum ys + 1

    let tileFloor = ((x_0, y_0), (x_1, y_1))

    putStrLn $ "Tile floor: " ++ show tileFloor



    let lns = map pointsOfLine $ zip posis (drop 1 posis ++ take 1 posis)

    let rs = combinations posis []



    let rectHasHits = or . (\r' -> map (any (pointInRectangle r')) lns)

    let hasHits = map rectHasHits rs
    let noHits = map not hasHits



    let rs' = map snd $ filter fst $ zip noHits rs



    putStr "\n"
    let enclosed = map (all rectHasHits . outerRectangle tileFloor) rs'

    let rs'' = map snd $ filter fst $ zip enclosed rs'


    print $ maximum $ map area rs''




  where

    -- the two larger rectangle, which difference will give the original rectangle
    -- second rectangle is the complete map
    outerRectangle :: Rectangle -> Rectangle -> [Rectangle]
    outerRectangle ((x_0, y_0), (x_1, y_1)) ((x0, y0), (x1, y1)) =
        let minX = min x0 x1
            maxX = max x0 x1
            minY = min y0 y1
            maxY = max y0 y1
        in
            [((x_0, minY), (maxX, y_1)), ((minX, y_0), (x_1, maxY))]

    area :: Rectangle -> Int
    area ((x0, y0), (x1, y1)) = 
        let dx = abs (x1 - x0)
            dy = abs (y1 - y0)
        in
            (dx + 1) * (dy + 1)