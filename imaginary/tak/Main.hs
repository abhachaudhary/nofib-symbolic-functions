import System.Environment

import G2.Symbolic

-- code of unknown provenance (partain 95/01/25)

tak :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int

-- SYMFUN: The following line makes use of symbolic function
tak f x y z = if f x y then z
       else tak f (tak f (x-1) y z)
		(tak f (y-1) z x)
		(tak f (z-1) x y)

main = do
    xs <- mkSymbolic
    ys <- mkSymbolic
    zs <- mkSymbolic
    symFun <- mkSymbolic
    print (tak symFun (xs) (ys) (zs))
