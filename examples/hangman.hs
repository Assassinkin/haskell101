-- This is to learn IO in Haskill
-- haskill is a purely functional language so it has no side effect whereas interacting with files has side effects cause we
-- are not sure the same output will be reached every time so haskill wnat to make sure to seprate the purely functional part
-- of the code from the IO as mush as possible
-- The Io type constructor produces types with an IO action wrapped around a value.
-- These differes from regular values( they can be evaluated inside a main function and special steps are required for
-- conversion: return reqular to IO , <- an IO to a regular action)
-- in this short program we will write the game of hangman
-- enable1.txt is a list of words used in scramble games you will need to downloaded from somewhere (it is free and availabl on the net just google it)
-- import the io module using:

import System.IO
import System.Random

main = do
  handle <- openFile "enable1.txt" ReadMode -- returns an IO function wrapped around the file
  contents <- hGetContents handle           -- extract the contents of the associated file into 1 long string
  gen <- getStdGen                          -- this is an IO call as it looks to some system aspect to het randeom stuff
  let words = map init (lines contents)     -- this is a fix to remove the extra c0aracter of newline
      (n, _) = randomR(0, (length words) - 1) gen :: (Int, StdGen) -- produces a random number in a specified range. _ is the next seed used to gen new number
      word = words !! n                                          -- and thats basically to keep track of the variables
  play word (map (\x -> '_') word) 6        -- replace all the letters with _
  -- putStrLn ("There are " ++ show (length(lines contents)) ++ "words.") -- build the print the string
  hClose handle                             --close the file

play word known guesses
  | word == known = do
      putStrLn known
      putStrLn "You win!!"
  | guesses == 0 = do
      putStrLn known
      putStrLn ("You lose. the word was " ++ word ++ ".")
  | otherwise    = do
      putStrLn known
      putStrLn ("You have " ++ show guesses ++ " guesses left.")
      line <- getLine
      let (newKnown, newGuesses) = handle (head line) word known guesses
      play word newKnown newGuesses

    --putStrLn (handle (head line) word) -- make sure to get the 1st caracter of the first line the user entered

handle letter word known guesses
  | letter `elem` word = (zipWith (\w k -> if w == letter then w else k) word known, guesses)
  | otherwise          = (known, guesses - 1)
