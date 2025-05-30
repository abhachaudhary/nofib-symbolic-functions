{-
    Haskell version of ...

!            Hope+ version of the Gabriel benchmark Boyer
! Started on 30th March 1988 by Tony Kitto

! Changes Log
! 07-04-88 ADK incorporated setup functions and tautp function
! 25-05-88 ADK changed to use LUT instead of assoclist for lemmas

Haskell version:

    23-06-93 JSM initial version

-}

module Main2 (main) where

import Lisplikefns
import Rewritefns
import Rulebasetext
import Checker

import System.Environment
import Control.Monad (forM_)

-- set-up functions for creating rulebase from text strings

lemmas :: LUT
lemmas = addlemmalst (makelemmas rules) newLUT

makelemmas :: [String] -> [Lisplist]
makelemmas []    = []
makelemmas (h:t) = mkLisplist (strToToken h) : (makelemmas t)

addlemmalst :: [Lisplist] -> LUT -> LUT
addlemmalst []    term = term
addlemmalst (h:t) term = addlemmalst t (addlemma h term)

addlemma :: Lisplist -> LUT -> LUT
addlemma Nil           term = term
addlemma (Atom x)      term = error "Atoms can't be lemmas"
addlemma (Cons (x, y)) term
    | tv x == "equal" && not (atom z) = addtoLUT (tv (car z), Cons(x, y), term)
    | otherwise 		      = error "Malformed lemma"
      where z = car y

{-
   Main function rewrites the test statement into canonical form
   and invokes the tautology checker
-}

tautp :: ((Lisplist, Lisplist) -> Bool) -> Lisplist -> Bool
tautp f term = tautologyp f (rewrite term lemmas, Nil, Nil)

{-
  The test statement and
  the substitution terms used to expand the test statement
-}

statement :: Lisplist

statement = mkLisplist (strToToken
                ("( implies ( and ( implies x y )\
                                \( and ( implies y z )\
                                      \( and ( implies z u )\
                                            \( implies u w ) ) ) )\
                          \( implies x w ) )"))

subterm :: Int -> Lisplist
subterm i = mkLisplist (strToToken str)
  where
    c = "c" ++ show i
    str = "( ( x f ( plus ( plus a b )\
                      \( plus " ++ c ++ " ( zero ) ) ) )\
                \( y f ( times ( times a b )\
                      \( plus " ++ c ++ " d ) ) )\
                \( z f ( reverse ( append ( append a b ) \
                      \( [] ) ) ) )\
                \(u equal ( plus a b ) ( difference x y ) )\
                \(w lessp ( remainder a b )\
                         \( member a ( length b ) ) ) )"

teststatement :: Int -> Lisplist
teststatement i = applysubst (subterm i) statement

testresult :: ((Lisplist, Lisplist) -> Bool) -> Int -> Bool
testresult f i = tautp f (teststatement i)

report :: Bool -> String
report True  = "The term is a tautology\n"
report False = "The term is not a tautology\n"

-- main = do
--   (n:_) <- getArgs
--   forM_ [1..read n] $ \i -> do
--     putStr (report (testresult i))

main symFun n = testresult symFun n
