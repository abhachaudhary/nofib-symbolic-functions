module Spectral.Minimax.Prog (prog) where

import Board
import Wins
import Game
import Tree


prog :: String -> String
prog input =
	"OXO\n" ++
	concat (map showMove game)
	where
	board "doesn't happen" = testBoard ++ testBoard
	board _                = testBoard
	game = alternate X max' min' (testBoard (stringToPiece input))


testBoard n = [[Empty,O,n],[Empty,X,Empty],[Empty,Empty,Empty]]

