-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Main2 (main) where
import System.Environment

myPrint d = if d > 0 then return "gt" else return "lt"

main arg symFun = nfib symFun arg

nfib :: (Double -> Double -> Double) -> Double -> Double
nfib f n = if n <= 1 then 1 else f (nfib f (n-1)) (nfib f (n-2)) + 1

