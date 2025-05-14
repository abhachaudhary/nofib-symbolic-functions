Compute the digits of "e" using continued fractions.
Original program due to Dale Thurston, Aug 2001

> module Main where
> import System.Environment (getArgs)

> import G2.Symbolic

> type ContFrac = [Integer]

Compute the decimal representation of e progressively.

A continued fraction expansion for e is

[2,1,2,1,1,4,1,1,6,1,...]

> eContFrac :: ContFrac
> eContFrac = 2:aux 2 where aux n = 1:n:1:aux (n+2)

We need a general function that applies an arbitrary linear fractional
transformation to a legal continued fraction, represented as a list of
positive integers.  The complicated guard is to see if we can output a
digit regardless of what the input is; i.e., to see if the interval
[1,infinity) is mapped into [k,k+1) for some k.

> -- ratTrans (a,b,c,d) x: compute (a + bx)/(c+dx) as a continued fraction
> ratTrans :: (Integer -> Integer -> Integer) -> (Integer,Integer,Integer,Integer) -> ContFrac -> ContFrac
> -- Output a digit if we can
> ratTrans f (a,b,c,d) xs |
>   ((signum c == signum d) || (abs c < abs d)) && -- No pole in range
>   (c+d)*q <= a+b && (c+d)*q + (c+d) > a+b       -- Next digit is determined
>      = q:ratTrans f (c,d,a-q*c,b-q*d) xs
> --  where q = b `div` d
> -- SYMFUN: The following line makes use of symbolic function
>   where q = f b d
> ratTrans f (a,b,c,d) (x:xs) = ratTrans f (b,a+x*b,d,c+x*d) xs

Finally, we convert a continued fraction to digits by repeatedly multiplying by 10.

> takeDigits :: (Integer -> Integer -> Integer) -> Int -> ContFrac -> [Integer]
> takeDigits _ 0 _ = []
> takeDigits f n (x:xs) = x:takeDigits f (n-1) (ratTrans f (10,0,0,1) xs)

> -- e :: Int -> [Integer]
> -- e n = takeDigits n eContFrac

> e' :: (Integer -> Integer -> Integer) -> Int -> [Integer]
> e' f n = takeDigits f n eContFrac

> -- main = replicateM_ 100 $ do
> --	digits <- mkSymbolic
> --	print (hash (show (e (digits))))

> main = do
>  symFun <- mkSymbolic
>  digits <- mkSymbolic
>  print (e' symFun digits)
