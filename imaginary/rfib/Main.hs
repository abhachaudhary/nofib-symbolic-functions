-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Main (main) where
import System.Environment

import G2.Symbolic

myPrint d = if d > 0 then return "gt" else return "lt"

main = do
	arg <- mkSymbolic
	symFun <- mkSymbolic
	myPrint ( nfib symFun arg)

nfib :: (Double -> Double -> Double) -> Double -> Double
nfib f n = if n <= 1 then 1 else f (nfib f (n-1)) (nfib f (n-2)) + 1

