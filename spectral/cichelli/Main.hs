module Main2 where

import Prog (prog)
-- Adding this import so that --validate works in G2
import Auxil
import System.Environment

main n symFun = prog symFun n