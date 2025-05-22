module Main2 where
	
import System.Environment

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

prime :: ([Int] -> [Int]) -> Int -> Int
-- SYMFUN: The following line makes use of symbolic function
prime f n = map head (iterate (f . the_filter) [2..n*n]) !! n

main arg symFun = prime symFun arg
