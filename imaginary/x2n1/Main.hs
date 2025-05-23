{-
Date: Tue, 15 Dec 92 14:39:56 +0100
From: Lennart Augustsson <augustss@cs.chalmers.se>
Message-Id: <9212151339.AA26402@animal.cs.chalmers.se>
To: partain@dcs.gla.ac.uk
Subject: Re: ghc 0.10 in animal:pub/incoming

...

I'd also like to contribute a small benchmark to your nofib suite, but it is
of the nfib kind:

<below>

It compute a root to the equation x^n = 1 (i.e. mkPolar 1 (2*pi/fromInt n)),
and raises it to the n:th power to get 1, sums a few of these and prints the
result.  The result of this program should be 10000.  It a reasonable test
of how well complex numbers are handled by the compiler.
My ulteriour motive for suggesting this benchmark is that the next version
of hbc will do pretty well on this example.  Since you have had the choice
of all the other programs I thought I'd contribute at least one :-)

...
-}

module Main2 ( main ) where
import Data.Complex hiding (mkPolar)
import System.Environment

main g h arg = round (realPart (sum [f g h n | n <- [1 .. (arg)]]))

f :: (Double -> Double) -> (Double -> Double) -> Int -> Complex Double
f g h n = mkPolar g h 1 ((2*pi)/fromIntegral n) ^ n

-- | Inlined and modified from Data.Complex
mkPolar          :: Floating a => (a -> a) -> (a -> a) -> a -> a -> Complex a
mkPolar g h r theta  =  r * g theta :+ r * h theta