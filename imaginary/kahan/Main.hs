{-# LANGUAGE FlexibleContexts #-}
-- Inferred type for 'inner' has a constraint (MArray (STUArray s) Double m)
-- An alternative fix (better, but less faithful to backward perf comparison)
-- would be MonoLocalBinds
--
-- SG: I tried adding MonoLocalBinds here and it didn't change anything in
-- -ddump-simpl. `inner` is probably properly specialised now. I think this
-- comment can go?!

-- | Implementation of Kahan summation algorithm that tests
-- performance of tight loops involving unboxed arrays and floating
-- point arithmetic.
module Main (main) where

import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST
import Data.Bits
import Data.Word
import System.Environment


vdim :: Int
vdim = 100

prng :: Word -> Word
prng w = w'
  where
    w1 = w `xor` (w `shiftL` 13)
    w2 = w1 `xor` (w1 `shiftR` 7)
    w' = w2 `xor` (w2 `shiftL` 17)

type Vec s = STUArray s Int Double

kahan :: (Word -> Word) -> Int -> Vec s -> Vec s -> ST s ()
kahan f vnum s c = do
    let inner w j
            | j < vdim  = do
                cj <- unsafeRead c j
                sj <- unsafeRead s j
                let y = fromIntegral w - cj
                    t = sj + y
                    --w' = prng w
                    -- SYMFUN: The following line makes use of symbolic function
                    w' = f w
                unsafeWrite c j ((t-sj)-y)
                unsafeWrite s j t
                inner w' (j+1)
            | otherwise = return ()

        outer i | i <= vnum = inner (fromIntegral i) 0 >> outer (i+1)
                | otherwise = return ()
    outer 1

{-calc :: Int -> ST s (Vec s)
calc vnum = do
    s <- newArray (0,vdim-1) 0
    c <- newArray (0,vdim-1) 0
    kahan vnum s c
    return s-}

calc' ::(Word -> Word) -> Int -> ST s (Vec s)
calc' f vnum = do
    s <- newArray (0,vdim-1) 0
    c <- newArray (0,vdim-1) 0
    kahan f vnum s c
    return s

main :: IO ()
main = do
    arg <- mkSymbolic
    symFun <- mkSymbolic
    -- Floating point benchmarks have unstable output across platforms, so
    -- we output the actual result.
    -- print . elems $ runSTUArray $ calc $ read arg
    runSTUArray (calc' symFun arg) `seq` return ()
