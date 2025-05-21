module Main2 (integrate1D, main) where

import System.Environment


integrate1D :: Double -> Double -> (Double->Double) -> Double
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum
      [ (f l)*0.5,
        f (l+d),
        f (l+(2.0*d)),
        f (l+(3.0*d)),
        f (l+(4.0*d)),
        f (u-(3.0*d)),
        f (u-(2.0*d)),
        f (u-d),
        (f u)*0.5]

integrate2D l1 u1 l2 u2 f = integrate1D l2 u2
            (\y->integrate1D l1 u1
              (\x->f x y))

zark u v = integrate2D 0.0 u 0.0 v (\x->(\y->x*y))

-- type signature required for compilers lacking the monomorphism restriction
ints = [1.0..] :: [Double]
-- SYMFUN: The following line makes use of symbolic function
zarks g = zipWith zark ints (map g ints)
rtotals g = head (zarks g) : zipWith (+) (tail (zarks g)) (rtotals g)
rtotal g n = rtotals g !! n

is = map (^4) ints
itotals = head is : zipWith (+) (tail is) itotals
itotal n = itotals!!n

es g = map (^2) (zipWith (-) (rtotals g) itotals)
--etotal n = sum (take n es)

etotal' g n = sum (take n (es g))

-- The (analytical) result should be zero
main range symFun = etotal' symFun range