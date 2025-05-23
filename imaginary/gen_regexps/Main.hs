-- !!! Wentworth's version of a program to generate
-- !!! all the expansions of a generalised regular expression
-- !!!
--
-- RJE: Modified so it only outputs the number of characters in the output,
-- rather that the output itself, thus avoiding having to generate such a
-- huge output file to get a reasonable execution time.

module Main2 (main) where

import Data.Char
import System.Environment

{-main = replicateM_ 500 $ do
  regex <- mkSymbolic
  print (hash (concat (expand regex)))-}

main regex symFun = concat (expand' regex symFun)

numchars :: [String] -> Int
numchars l = sum $ map length l

expand' f [] = [""]
expand' f ('<':x) = numericRule f x
expand' f ('[':x) = alphabeticRule f x
expand' f x = constantRule f x

{-expand [] = [""]
expand ('<':x) = numericRule x
expand ('[':x) = alphabeticRule x
expand x = constantRule x-}

constantRule f (c:rest) = [ c:z | z <- expand' f rest ]

alphabeticRule f (a:'-':b:']':rest)
  | a <= b  	= [c:z | c <- f [a..b],	      z <- expand' f rest]
  -- SYMFUN: The following line makes use of symbolic function
  | otherwise	= [c:z | c <- f [b..a], z <- expand' f rest]

numericRule f x
  = [ pad (show i) ++ z
	| i <- if u < v then [u..v] else [u,u-1..v]
	, z <- expand' f s ]
  where
    (p,_:q) = span (/= '-') x
    (r,_:s) = span (/= '>') q
    (u,v)   = (mknum p, mknum r)
    mknum s = foldl (\ u c -> u * 10 + (ord c - ord '0')) 0 s
    pad s   = [ '0' | i <- [1 .. (width-(length s))]] ++ s
    width   = max (length (show u)) (length (show v))
