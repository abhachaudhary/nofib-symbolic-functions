import System.Environment

import G2.Symbolic

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

prime :: ([Int] -> [Int]) -> Int -> Int
prime f n = map head (iterate f [2..n*n]) !! n

main = do
	arg <- mkSymbolic
	symFun <- mkSymbolic
	print $ prime symFun arg
