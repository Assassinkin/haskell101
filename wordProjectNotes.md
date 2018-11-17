# [Words](https://github.com/Assassinkin/wordsGridGame) "Project" notes 

`stack setup`: run from outside a project using implicit global config.

Run `stack ghci` instead of `ghci` so you can use the latest ghci istalled by stack.

Build a haskell program from a file: `stack ghc file.hs`.

Build uisng dynamic linking: `stack ghc -- -dynamic test.hs`.

To create a project with stack run:

```
stack new words new-template -p "author-email:iuns@outlook.fr" -p "author-name:KaliAWSfatE" -p "category:dev" -p "copyright:BSD" -p "github-username:assassinKin"`
To compile it use : `cd words; stack ghci
```

To create a module: use the `module` keyword then the name of the variable `Lib` in this case. then a list of the exported functions in `(someFunc)` and finally the `where` keyword to define the functions and symbols.

here is an example:
```
module Lib
    ( someFunc
    , someString
    ) where

someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"
```

`stack ghci` : compile the code for us  and open ghci.

To build the project run `stack build` then run `stack exec words-exe` to execute the binary.

We can change the project metadata, executable name, ... in the `words.cabal` file.

We can add `-dynamic` in the `ghc-options` in the `words.cabal` file to make sure the binary size is reasonable

To run tests: `stack test`.

When importing modules we need to specify that in the words.cabal file: add it to the `build-depends` section.

If the module is needed for testing make sure to import it under `build-depends` for `test-suite` .

In the test file: `$` in `main = hspec $ do` is a syntaxic sugar that replaces ().

Hoogle is a usefull haskell api search engine.

If we need a function that have a specific type def we can search for it via [hoogle](https://www.haskell.org/hoogle/) .We need just to type `[String] -> String`.

we can access a position i a list via `[1 ..] !! 50`.

we can get a list of the 10 first value in a list via `take 10 [1 ..]`.

also this is usefull: `takeWhile (10) [0 ..]`.
