{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main where
import Data.Array.IArray (array, (!), (!?), Array, assocs, (//), bounds)
--import qualified Data.Array.IArray (elems)
--import Data.Array.Base (UArray)
import Data.Maybe (fromMaybe)

type RollsMap = Array (Int, Int) Char


main :: IO ()
main = do
    part2

part1 :: IO ()
part1 = do
    input <- readFile "example.txt"
    --input <- readFile "puzzle_input.txt"
    let rows = lines input


    let numCols = length $ head rows
    let numRows = length rows
    let numberedRows = zip [1..] rows :: [(Int, String)]
    let elems = concatMap (\(nrow, row) -> zipWith (\ncol val -> ((ncol, nrow), val)) [1..] row) numberedRows

    let theMap = array ((1,1), (numCols, numRows)) elems :: RollsMap

    print $ bounds theMap

    -- let theLargerMap = array ((0, 0), (numCols+1, numRows+1)) [
    --              ((x, y), fromMaybe 'W' $ theMap !? (x,y)) | x <- [0..numCols+1], y <- [0..numRows+1]
    --          ] :: Array (Int, Int) Char

    
    
    --print $ indices theMap
    -- let paperPosis = map fst $ filter (\(_, val) -> val == '@') $ assocs theLargerMap

    -- let nbs = zip paperPosis $ map (length . filter (=='@') . neighbors theLargerMap) paperPosis
    -- let valid = map fst $ filter ((<4) . snd) nbs
    --let valid = getValidPosis theMap

    --let theMap' = theMap // zip valid (repeat 'x')

    let (theMap', num) = reduceRollsMap 'x' theMap

    let output = [fromMaybe '\n' $ theMap' !? (x,y) | y <- [1..numRows] , x <- [1..numCols+1]]

    putStrLn "\n"
    putStr output
    print num
  

  
stencil :: [(Int, Int)]
stencil = [
        (-1, -1), (0, -1), (1, -1),
        (-1,  0),          (1,  0),
        (-1,  1), (0,  1), (1,  1)
    ]

neighbors :: RollsMap -> (Int, Int) -> [Char]
neighbors mp (x, y) = map (mp!) $ map (\(dx, dy) -> (x + dx, y + dy)) stencil

getValidPosis :: RollsMap-> [(Int, Int)]
getValidPosis mp = 
    let (_, (nx, ny)) = bounds mp
        largerMp = array ((0, 0), (nx+1, ny+1)) [((x, y), fromMaybe 'W' $ mp !? (x,y)) | x <- [0..nx+1], y <- [0..ny+1]]
        pPosis = map fst $ filter (\(_, val) -> val == '@') $ assocs largerMp
        nbs = zip pPosis $ map (length . filter (=='@') . neighbors largerMp) pPosis
    in
        map fst $ filter ((<4) . snd) nbs

reduceRollsMap :: Char -> RollsMap -> (RollsMap, Int)
reduceRollsMap c mp = 
    let 
        valid = getValidPosis mp
        mp' = mp // zip valid (repeat c)
    in 
        (mp', length valid)

showRolllMap :: RollsMap -> String
showRolllMap mp =
    let
        (_, (nx, ny)) = bounds mp
    in
        [fromMaybe '\n' $ mp !? (x,y) | y <- [1..ny] , x <- [1..nx+1]]



-- ---------------------------------------------------


part2 :: IO ()
part2 = do
    --input <- readFile "example.txt"
    input <- readFile "puzzle_input.txt"
    let rows = lines input
    let numCols = length $ head rows
    let numRows = length rows
    let numberedRows = zip [1..] rows :: [(Int, String)]
    let elems = concatMap (\(nrow, row) -> zipWith (\ncol val -> ((ncol, nrow), val)) [1..] row) numberedRows

    let theMap = array ((1,1), (numCols, numRows)) elems :: Array (Int, Int) Char

    let result = reduceTillEnd [(theMap, 0)]

    putStr $ showRolllMap $ fst $ head result
    print $ sum $ map snd result



reduceTillEnd :: [(RollsMap, Int)] -> [(RollsMap, Int)]
reduceTillEnd mps = 
    let 
        (mp, _) = head mps
        (mp', num') = reduceRollsMap '.' mp
        mps' = (mp', num'):mps
    in
        if num' == 0 then mps' else reduceTillEnd mps'
