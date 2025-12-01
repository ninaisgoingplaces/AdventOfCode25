module Main where


main :: IO ()
main = do
    part2

applyCode :: Integer -> String -> Integer
applyCode n ('R':rest) = n + read rest
applyCode n ('L':rest) = n - read rest
applyCode _ _ = undefined

-- scanl :: (b -> a -> b) -> b -> [a] -> [b] 

part1 :: IO ()
part1 = do
    input <- readFile "example.txt"
    --input <- readFile "puzzle_input.txt"
    let result = drop 1 $ scanl applyCode 50 $ lines input
    let result' = map (\x -> mod x 100) result
    print result
    print result'

    let nulls = filter (== 0) result'
    print nulls
    print $ length nulls


{-
That gives all zeros between both end-point (excluded):
print $ zerosFromTo (-80) 200
> [0,100]
-}
zerosFromTo :: Integer -> Integer -> [Integer]
zerosFromTo a b = 
    let 
        d0 = a `div` 100
        d1 = b `div` 100
        ns = if b > a then [d0..d1] else [d1..d0]
        between x0 x1 x | x0 < x1 = x > x0 && x < x1
        between x0 x1 x           = between x1 x0 x
    in
        filter (between a b) $ map (\x -> 100 * x) ns


part2 :: IO ()
part2 = do
    --input <- readFile "example.txt"
    input <- readFile "puzzle_input.txt"
    let result = scanl applyCode 50 $ lines input
    let zeros = drop 1 $ map (\x -> mod x 100) result

    print $ zerosFromTo (-80) 200

    let innerClicks = map length $ zipWith zerosFromTo result (drop 1 result)
    let numEndClicks = length $ filter (== 0) zeros
    let numInnerClicks = sum innerClicks

    print $ numEndClicks + numInnerClicks