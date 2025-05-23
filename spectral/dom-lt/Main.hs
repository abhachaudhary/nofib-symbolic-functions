{-# OPTIONS_GHC -fno-full-laziness #-}

module Main2 where

import System.Environment
import Data.Bifunctor

import Dom
import Control.Monad
import Data.Traversable

-- Take a filename as input.
-- Each line is expected to be a graph of the form (root, [(vertex, [successors])])
-- Compute the dominators for each line.
-- main :: IO ()
-- main = do
--     [inputFile,repetitions] <- getArgs
--     let repetitions' = read repetitions

--     sgraphs <- map (second fromAdj . read) . lines <$> readFile inputFile :: IO [Rooted]
--     let s = flip map [0..repetitions'] $ (\i -> sum (map (\g -> i + doGraph g) sgraphs)) :: [Int]
--     print $ sum s

main lines n = 
    let
        sgraphs = map (second fromAdj) lines
        sum1 = sum (map (\g -> n + doGraph g) sgraphs)
    in 
        sum1


doGraph :: Rooted -> Int
doGraph g = length . idom $ g