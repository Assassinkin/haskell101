#!/usr/bin/env runhaskell

import		 Data.Ord
import		 Data.Char
import 		 Data.List
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import		 System.Environment

main = do
[fp] <- getArgs
text <-T.readFile fp
let histo = 
	foldl'
	  (\hash line -> M.insertWith (+) (T.length line) 1 hash)
	  M.empty
	  (T.lines text)
mapM_ (\(key, value) -> print (key, value))
--	(sortBy (comparing snd) (M.toList histo)) -- from the least to the most 
	(sortBy (flip (comparing snd)) (M.toList histo))
