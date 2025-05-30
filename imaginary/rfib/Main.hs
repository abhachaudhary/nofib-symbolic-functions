-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Main2 where
import System.Environment

main arg symFun = nfib symFun arg

nfib :: (Double -> Double -> Double) -> Double -> Double
nfib f n = if n <= 1 then 1 else f (nfib f (n-1)) (nfib f (n-2)) + 1

