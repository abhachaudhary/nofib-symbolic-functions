module Main where

import Prog (prog)
import System.Environment

import G2.Symbolic

main = do
    n <- mkSymbolic
    symFun <- mkSymbolic
    putStr (prog symFun n)
