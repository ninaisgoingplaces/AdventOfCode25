{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Main where

import Data.Array.IArray (array, (!), Array, assocs, (//), bounds)
import Data.List (sortBy, sortOn, foldl', sort)
import Data.Map (Map, (!?), insert, update, size, empty, lookup, toList)
import Control.Monad (forM_)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import GHC.List (foldr')
import qualified Data.Ord


split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (drop 1 ls)
        where
          (f,ls) = span (/=e) l

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

main :: IO ()
main = do
    part2



part1 :: IO ()
part1 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"
    let vals = map (map read . split ',') rows :: [[Int]]
    let num = length vals

    let jncts = array (0, num-1) $ zip [0..] vals :: Array Int [Int]

    let sqDsts = map (\p -> (p, uncurry distanceSq . both (jncts !) $ p)) $ pairs num

    let stDs = map fst $ sortOn snd sqDsts

    -- forM_ [314..315] (\i -> do {
    --      print i;
    --      print $ last $ take i stDs;
    --      print $ snd $ run1 (take i stDs) (empty, empty);
    --  })




    --let vals = take 315 stDs

    let vals = take 1000 stDs

    let res = sortOn (\x -> - length x) $ map snd $ toList $ snd $ run1 vals (empty, empty)

    -- print $ take 3 res
    putStr "\n"
    --print res 
    print $ product $ map length $ take 3 res



distanceSq :: [Int] -> [Int] -> Int
distanceSq [x0, y0, z0] [x1, y1, z1] = (x0-x1)^2 + (y0-y1)^2 + (z0-z1)^2

pairs :: Int -> [(Int, Int)]
pairs num = do
    i <- [0..num-2]
    j <- [i+1..num-1]
    return (i, j)

type DistanceList =  [((Int, Int), Int)]

type MapJnct2Crct = Map Int Int

type MapCrct2Jnct = Map Int [Int]


run1 :: [(Int, Int)] -> (MapJnct2Crct, MapCrct2Jnct) -> (MapJnct2Crct, MapCrct2Jnct)
run1 []              res        = res
run1 ((i,j):dls) (j2c, c2j) =
        run1 dls $ next (length dls) (i, j) (j2c, c2j)

next :: Int -> (Int, Int) -> (MapJnct2Crct, MapCrct2Jnct) -> (MapJnct2Crct, MapCrct2Jnct)
next stepNum (i,j) (j2c, c2j) = step (j2c !? i) (j2c !? j)
    where
        step :: Maybe Int -> Maybe Int -> (MapJnct2Crct, MapCrct2Jnct)
        step (Just ci) (Just cj) | ci == cj = (j2c, c2j)
        step (Just ci) (Just cj)            =
            -- we need to merge two circuits and choose ci as the combined circuit
            let
                toMove = fromJust $ lookup cj c2j
                -- map all junctions found in circuit cj to circuit ci
                j2c' = foldl' (\jm n -> update (const $ Just ci) n jm) j2c toMove
                --j2c' = foldr' (\n jm -> update (const $ Just ci) n jm) j2c toMove
                -- insert all junctions in new cicrcuit
                c2j' = update (\x -> Just $ x ++ toMove) ci c2j
                -- delete old circuit
                c2j'' = update (const Nothing) cj c2j'

            in
                (j2c', c2j'')


        step (Just ci) Nothing    = (insert j ci j2c, update (\x -> Just (j:x)) ci c2j)
        step Nothing   (Just cj)  = (insert i cj j2c, update (\x -> Just (i:x)) cj c2j)
        step Nothing   Nothing    =
            let
                cn    = stepNum -- take the length of the rest as an monotonical increasing identifier
                j2c'  = insert i cn j2c
                j2c'' = insert j cn j2c'
                c2j'  = insert cn [i,j] c2j
            in
                (j2c'', c2j')

-- --------------------------

part2 :: IO ()
part2 = do
    --rows <- lines <$> readFile "example.txt"
    rows <- lines <$> readFile "puzzle_input.txt"

    let vals = map (map read . split ',') rows :: [[Int]]
    let num = length vals

    let jncts = array (0, num-1) $ zip [0..] vals :: Array Int [Int]

    let sqDsts = map (\p -> (p, uncurry distanceSq . both (jncts !) $ p)) $ pairs num
    let vals = map fst $ sortOn snd sqDsts

    let res = run2 num vals (empty, empty) Nothing


    case res of
        Nothing     -> print "Nope"
        Just (i, j) -> (
            let x0 = head $ jncts ! i
                x1 = head $ jncts ! j
            in
                print $ x0 * x1)


run2 :: Int -> [(Int, Int)] -> (MapJnct2Crct, MapCrct2Jnct) -> Maybe (Int, Int) -> Maybe (Int, Int)
run2 _            []          maps      lastPair  = lastPair
run2 numJunctions ((i,j):dls) (j2c, c2j) lastPair | largestCircuit c2j < numJunctions =
        run2 numJunctions dls (next (length dls) (i, j) (j2c, c2j)) (Just (i,j))
run2 _            _           maps lastPair = lastPair

largestCircuit :: MapCrct2Jnct -> Int
largestCircuit cm =
    let
        circuits = map snd $ toList cm
        sorted = sortOn (Data.Ord.Down . length) circuits
    in
        if sorted /= [] then length $ head sorted else 0