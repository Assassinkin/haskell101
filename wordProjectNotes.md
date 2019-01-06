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


When writing a module make sure to export only the functions needed by other programs, no need to export helper functions.

We can also selectively import functions:
```Haskell
import Data.Char (toLower,isSpace,isPunctuation)
```

When importing we use qualified imports as follow:
```Haskell
 import qualified Data.Text as T
 ```

 Or we can drop the `as` part and use the module name directly.
```Haskell
import qualified Blabla
```

Getting rid of language pragmas: Add this in the .cabal file after the `default-language` entry:  `extensions: OverloadedStrings`

To set a compiler flag to warn of any potential problems with the code, add `-Wall` flag. For stack add the flag to the `ghc-options` in the executable section of the `.cabal` file.

To not miss warnings on projects, compile with `-error`, which causes an error anytime a warning is found.
