import Data.List
import Data.Maybe
import Utilities

-- The empty board
emptyBoard = [ "...","...","..." ]

-- Returns the winner 
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
       | line /=Nothing                 = head (fromJust line)
       | '.' `elem` [a,b,c,d,e,f,g,h,i] = '?' -- Game not over
       | otherwise                      = '-' -- Tie
       where line = find (\[x,y,z] -> x == y && y == z && z /= '.')
                         [[a,b,c],[d,e,f],[g,h,i],[a,d,g],[b,e,h],[c,f,i],[a,e,i],[c,e,g]]
-- /= means !=
-- Replaces the caracter at position r, c in board
play :: Int -> Int -> Char -> [[Char]] -> [[Char]]
play r c a board = replace r (replace c a (board !! r)) board

-- Return a list of succeding board when player play
successors :: Char -> [[Char]] -> [[[Char]]]
successors player board = [play r c player board | r <- [0..2], c <- [0..2], board !! r !! c =='.']

-- Returns the value of board with player to play
value :: Char -> [[Char]] -> Int
value player board
      | w == 'X'      = 1
      | w == 'O'      = -1
      | w == '-'      = 0
      | player == 'X' = maximum (map (value 'O') (successors 'X' board))
      | otherwise     = minimum (map (value 'X') (successors 'O' board))
      where w = winner board

-- Returns board after X's best move
bestMove :: [[Char]] -> [[Char]]
bestMove board = argmax (value 'O') (successors 'X' board)
