# haskell101
My notes while learning Haskell

---

### Version: 0.1.0
***

* This repo contains some resources i found interesting to start with learning Haskell
* my personal notes and a cheat sheet
* 3 illustrative games examples:
  - Hangman
  - Tictac [WIP]

---

### Play Hangman:
Run the program using : `runhaskell hangman.hs`

### play Tictac
Run the haskell interpreter and load the programm
```Prelude> :l tictac.hs
*Main> bestMove emptyBoard
["X..","...","..."]
*Main> play 1 2 'O' it
["X..","..O","..."]
*Main> bestMove it
["X.X","..O","..."]
*Main> play 0 1 'O' it
["XOX","..O","..."]
*Main> bestMove it
["XOX",".XO","..."]
*Main> play 2 2 'O' it
["XOX",".XO","..O"]
*Main> bestMove it
["XOX",".XO","X.O"]
*Main> play 1 0 'O' it
["XOX","OXO","X.O"]
*Main> bestMove it
["XOX","OXO","XXO"]
```

TODO: Refactor it into a program with a main.

## Resources:

[Haskell Tutorial](https://www.youtube.com/watch?v=02_H3LjqMr8&t=26s)
[Cheat Sheet](http://www.newthinktank.com/2015/08/learn-haskell-one-video/)
[Best Free Haskell Book](http://learnyouahaskell.com/chapters)
[Learn You a Haskell](https://www.youtube.com/playlist?list=PLS6urCrsYES24Fwzg5-Uga1QEbNm9kiU_)
