module Main (main) where

import System.Environment
import G2.Symbolic

main = do
  --runalltests 1 20 10000 1 20 10000
  --runalltests (-1000000) 4001 1000000 (-1000000) 4001 1000000
  --runalltests (-2100000000) 4000001 2100000000 (-2100000000) 4000001 2100000000
  _1 <- mkSymbolic
  _2 <- mkSymbolic
  _3 <- mkSymbolic

  let astart = read _1; astep = read _2; alim = read _3
  runalltests astart astep alim astart astep alim

runalltests
	:: Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
runalltests astart astep alim bstart bstep blim = do
  runbench f (+) (+) "(+)" astart astep alim astart astep alim
  runbench f (-) (-) "(-)" astart astep alim astart astep alim
  runbench f (*) (*) "(*)" astart astep alim astart astep alim

  runbench f div div "div" astart astep alim astart astep alim
  runbench f mod mod "mod" astart astep alim astart astep alim
  runbench f quot quot "quot" astart astep alim astart astep alim
  runbench f rem rem "rem" astart astep alim astart astep alim

  runbench f gcd gcd "gcd" astart astep alim astart astep alim
  runbench f lcm lcm "lcm" astart astep alim astart astep alim

  runbench f (==) (==) "(==)" astart astep alim astart astep alim
  runbench f (<) (<) "(<)" astart astep alim astart astep alim
  runbench f (<=) (<=) "(<=)" astart astep alim astart astep alim
  runbench f (>) (>) "(>)" astart astep alim astart astep alim
  runbench f (>=) (>=) "(>=)" astart astep alim astart astep alim

runbench
	:: (Integer -> a)
  -> (Integer -> Integer -> a)
	-> (Int -> Int -> b)
	-> String
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
runbench f jop iop opstr astart astep alim bstart bstep blim = do
 intbench f iop astart astep alim astart astep alim
 integerbench jop astart astep alim astart astep alim

integerbench :: (Integer -> Integer -> a)
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
integerbench op astart astep alim bstart bstep blim = do
  seqlist ([ a `op` b
	   | a <- [ astart,astart+astep..alim ]
	   , b <- [ bstart,astart+bstep..blim ]])
  return ()

intbench :: (Integer -> a)
  -> (Int -> Int -> a)
	-> Integer -> Integer -> Integer
	-> Integer -> Integer -> Integer
	-> IO ()
intbench f op astart astep alim bstart bstep blim = do
  seqlist ([ a `op` b
	   | a <- [ f astart,f astart + f astep .. f alim ]
	   , b <- [ f bstart,f astart + f bstep .. f blim ]])
  return ()

seqlist [] = return ()
seqlist (x:xs) = x `seq` seqlist xs
