-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

module Main2 where

import System.Environment

main arg symFun = nsoln symFun arg

nsoln f nq = length (gen f nq)
 where
    safe :: (Int -> Int -> Bool) -> Int -> Int -> [Int] -> Bool
    safe f x d []    = True
    -- SYMFUN: The following line makes use of symbolic function
    safe f x d (q1:q2:l) = (f x q1) && x /= q1+d && x /= q1-d &&
                           not (f x q2) && x /= q2+d && x /= q2-d && safe f x (d+1) l
    safe f x d (q:l) = (f x q) && x /= q+d && x /= q-d && safe f x (d+1) l

    gen :: (Int -> Int -> Bool) -> Int -> [[Int]]
    gen f 0 = [[]]
    gen f n = [ (q:b) | b <- gen f (n-1), q <- [1..nq], safe f q 1 b]
