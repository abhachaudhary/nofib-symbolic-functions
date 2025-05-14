-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Main (main) where
import System.Environment

import G2.Symbolic

main = do
	arg <- mkSymbolic
	print $ nfib $ arg

nfib :: Double -> Double
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1

