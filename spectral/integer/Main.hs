module Main2 (main) where

import System.Environment

main f1 f2 f3 g1 g2 g3 _1 _2 _3 = 
  --runalltests 1 20 10000 1 20 10000
  --runalltests (-1000000) 4001 1000000 (-1000000) 4001 1000000
  --runalltests (-2100000000) 4000001 2100000000 (-2100000000) 4000001 2100000000
  let astart = _1; astep = _2; alim = _3 in
  runalltests f1 f2 f3 g1 g2 g3 astart astep alim astart astep alim

runalltests
	:: (Integer -> Integer -> a)
  -> (Integer -> Integer -> a)
  -> (Integer -> Integer -> a)
	-> (Int -> Int -> b)
	-> (Int -> Int -> b)
	-> (Int -> Int -> b)
  -> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> ()
runalltests f1 f2 f3 g1 g2 g3 astart astep alim bstart bstep blim =
  runbench f1 g1 "(+)" astart astep alim astart astep alim
  `seq`
  runbench f2 g2 "(-)" astart astep alim astart astep alim
  `seq`
  runbench f3 g3 "(*)" astart astep alim astart astep alim

  -- runbench div div "div" astart astep alim astart astep alim
  -- runbench mod mod "mod" astart astep alim astart astep alim
  -- runbench quot quot "quot" astart astep alim astart astep alim
  -- runbench rem rem "rem" astart astep alim astart astep alim

  -- runbench gcd gcd "gcd" astart astep alim astart astep alim
  -- runbench lcm lcm "lcm" astart astep alim astart astep alim

  -- runbench (==) (==) "(==)" astart astep alim astart astep alim
  -- runbench (<) (<) "(<)" astart astep alim astart astep alim
  -- runbench (<=) (<=) "(<=)" astart astep alim astart astep alim
  -- runbench (>) (>) "(>)" astart astep alim astart astep alim
  -- runbench (>=) (>=) "(>=)" astart astep alim astart astep alim

runbench
	:: (Integer -> Integer -> a)
	-> (Int -> Int -> b)
	-> String
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> ()
runbench jop iop opstr astart astep alim bstart bstep blim =
 intbench iop astart astep alim astart astep alim
 `seq`
 integerbench jop astart astep alim astart astep alim

integerbench :: (Integer -> Integer -> a)
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> ()
integerbench op astart astep alim bstart bstep blim =
  ([ a `op` b
	   | a <- [ astart,astart+astep..alim ]
	   , b <- [ bstart,astart+bstep..blim ]])
  `seq`
  ()

intbench :: (Int -> Int -> a)
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> ()
intbench op astart astep alim bstart bstep blim =
  seqlist ([ a `op` b
	   | a <- [ fromInteger astart,fromInteger astart + fromInteger astep .. fromInteger alim ]
	   , b <- [ fromInteger bstart,fromInteger astart + fromInteger bstep .. fromInteger blim ]])

seqlist [] = ()
seqlist (x:xs) = x `seq` seqlist xs
