{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

--import Data.Tuple.Extra (second)




main :: IO ()
main = do
    part1

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l


innerString :: String -> String
innerString s = take (length s - 2) $ drop 1 s

getShape :: [String] -> (Int, [String])
getShape []     = undefined
getShape (x:xs) = (read $ take 1 x, xs)

shapeSize :: [String] -> Int
shapeSize = sum . map (length . filter (=='#'))

getRegion :: String -> ([Int], [Int])
getRegion s =
    let ps = split ' ' s
        vals = map read $ drop 1 ps
        ds = map read $ split 'x' $ (\x -> take (length x -1) x) $ head ps
    in
        (ds, vals)


part1 :: IO ()
part1 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let parts = split "" rows
    let shapes = map getShape (take 6 parts)
    let sizes = map (shapeSize . snd) shapes

    let regions = map getRegion $ parts !! 6

    let (ds, vals) = regions !! 1

    print $ (ds !! 0) * (ds !! 1)
    print $ sum $ zipWith (*) vals sizes

    print sizes

    print $ canFit sizes $ regions !! 0

    let res = map (canFit sizes) regions

    print $ length $ filter id res
    print $ length $ filter not res


    putStrLn ""

  where
    canFit :: [Int] -> ([Int], [Int]) -> Bool
    canFit sizes (ds, vals) = 
        let
            rSize = (ds !! 0) * (ds !! 1)
            pSize = sum $ zipWith (*) vals sizes
        in
            pSize <= rSize

-- --------------------------

