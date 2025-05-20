module Main2 where

import Control.Monad (replicateM_)
import System.Environment (getArgs)
import Spectral.Minimax.Prog (prog)

-- #ifdef PAR
-- main input = prog input
-- #else
-- suspect:main ~((Str str):_) = [ReadChan stdin, AppendChan stdout (prog str)]
{-main = do
	(n:_) <- getArgs
	replicateM_ (read n) $ do
		(s:_) <- getArgs
		length (prog s) `seq` return ()-}

main s = length (prog s)
-- #endif
