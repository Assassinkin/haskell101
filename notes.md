# Download Haskell Stack

wget -qO- https://get.haskellstack.org/ | sh

stack is similar to nvm or yarn from the NodeJS world

it is good for the following:

Having different versions of the Haskell compiler (i.e. ghc) available on your machine without messing things up, and using the right ghc version for your project.
Taking care of which Haskell libraries are known to compile/build with which version of ghc.
Taking care of the dependency graph of libraries, so that all the libraries that your project depends on, compile successfully without you having to manually specify the version of each library. Basically stack saves you from the dependency hell problem.


* Setup your first throw-away project:

$ stack new --resolver=lts-9.14 first-project
$ cd first-project
$ stack setup

stack setup may take forever, becuase it will probably download the GHC compiler and a bunch of core/base libraries.

### Hackage vs Stackage & Cabal vs Stack
Hackage is the original package repository. This is where authors upload their packages to share them with the world.
Stackage pulls specific versions of specific packages from Hackage and divides them into “sets” known as “snapshots”. Each snapshot contains only a single version of any package with a guarantee that all the packages in a given snapshot will successfully build when included in a project. That is, you will not get a dependency hell when your project depends on 5 packages from the same Stackage snapshot. (If you go to a snapshot/LTS’ listing page you can verify that there is only one version of each package listed there. On the other hand, if you check the same package on Hackage, you will see all versions of the package listed there).
Hackage has way more packages than Stackage, because, not every author adds their package to the Stackage snapshot. This is probably because, every time a new LTS/snapshot is released, package-authors have to do some extra work to maintain the “no dependeny-hell guaranteee”. However, most popular/important packages have already been added to Stackage, so you won’t be missing any packages till you start doing advanced Haskell stuff.
The command-line tool called cabal does not know about Stackage and pulls packages directly from Hackage. Also, cabal+Hackage do not give this “no dependency-hell guarantee” that stack+Stackage works very hard to maintain.
The command-line tool called stack pulls packages from Stackage, by default. Having said that, it can pull packages from Hackage if a particular package is not available in its snapshot (but this requires a few extra lines of config).



### No “truthy” or “falsy” values
Unlike other languages, Haskell doesn’t have the concept of truthy and falsy values. Some other languages treat a number of non-boolean values, like 0, 1, any string, empty array, empty object, etc, as either a boolean True or a boolean False. Haskell doesn’t do that. Haskell has only two boolean values - True or False. This is a result of the strong & principled type-system of Haskell.



A Haskell module is a collection of related functions, types and typeclasses. A Haskell program is a collection of modules where the main module loads up the other modules and then uses the functions defined in them to do something.


*** to have multiple function in a file just write them up under each other with no need for module key word.

### Commonly used higher-order functions in Haskell

*map*: This higher-order function abstracts out the procedure of going through every element in a list and transforming it to a new value. That’s it. You cannot change the order of elements. You cannot skip elements. You cannot add elements. The lambda that you pass, will be run on every single element of the list and the output values will be collected in a list and returned. Which means, that the length of the input list and output list will always be the same.


type signature:
```
GHCi> :t map
map :: (a -> b) -> [a] -> [b]
```
This is what is called a polymorphic function in Haskell. Any type that is represented by lowercase identifiers in the type-signature, like a, b, server, file, instead of Server or File, will work if you pass values of any type to it. However, these “type-variables” will specify a relationship between the various arguments of the function.

**note** Whenever you see parentheses in a type-signature, remember that you need to pass a function/lambda of that shape/type to that argument.

*filter*

Filter allows us to select elements, which satisy a given condition, from a list. It is abstracts away the repetitive procedure of going through every element in an input list, checking each element against a condition, and collecting all elements which pass that condition into a new list.

*foldl'*
The strangely named foldl' is an extremely important higher-order function. In some senses, it is the “mother function” of all higher-order functions that deal with processing lists. Using foldl' you can implement, map, filter, and every other such function that visits each element and do something with it.

if you need to access the result of the element already processed filter and map are no good.

some code:  
```let list = [1, 20 ..]
    take 10 list
take 10 element from the 'infinite' list
```

```checkNumber :: Int -> Int
checkNumber y =
 if (mod y 2) == 0
   then 0
   else 1

listEvenSum :: Int -> [Int] -> Int
listEvenSum total list =
  if (list == [])
    then total
    else if (mod (head list) 2) == 0
      then listEvenSum (total + (head list)) (tail list)
      else listEvenSum total (tail list)

```

will keep etirating over the list and adding the head to the total until the list is empty then we can return the total (recursive stuff)




### high order function:
```
import Data.Char (toUpper)
map (\x -> toUpper x) "abc"
```

fold will loop through each element of the list (x) and sum it with total which starts with 0 in general and keep adding elements from the list
foldl' (\total x -> total + x) 0 [1, 2, 3, 4]

check if the result hold that number if so it is ignored else it is added

GHCi> foldl' (\result x -> if (x `elem` result) then result else (x:result)) [] [1, 2, 3, 3, 2, 3, 1]
5 `elem` [1,5,3]


### Detour: Tuples

 If at the time of writing the code (compile-time) you know that a particular function will always return two values, you can encode that guarantee by picking the right type ie a tuple of x element (x-tuple)


 this
 ```
 [ "a", ["Adam", "Anurag", "Ashtong"]
, "b", ["Bhushan", "Bimal", "Banksky", "Beth"
, "c", ["Charlie", "Chaman", "Chilgoza"]
]
```
won't work in haskell cause ll elements in a list should be of the same type. However, in the data-structure given above, the list elements are alternating between `String` and `[String]`.
best way for this to work is using a list of 2-tuple:
```
[ ('a', ["Adam", "Anurag", "Ashtong"])
, ('b', ["Bhushan", "Bimal", "Banksky", "Beth"])
, ('c', ["Charlie", "Chaman", "Chilgoza"])
]
```

### Algebraic Data Types

To be done ...

# again  

to foce haskell to treat a var in a certain way use:
(123 :: Float) then 123 will be considered float


let x = 'h' this wont work cause the double ' is used for char not strings


use :type (:t) to get the type of a variable
use :info  to get info on some variable type (Int Float etc ...)
use :help for the full list


A list is an empty list "[]" or a variable appended to another list
1: [2] outputs [1, 2]

### dealing with main functions:
main function is of the type input output and it does not return a value

we can compile the program using `stack ghi main.hs` and it will output a binary file
the output file would be large aroud 10 mB which is not ok for a hello world module this is due ghi linking all thje module used by haskell
to prevent that we can use `stack ghi -- -dynamic test.hs`

### Intro to function (again :p)
add a b = a + b is similar to add a b = (+) a b cause (+) is a function in itself

### Data Structure

Type synonym are useful for readability but they don't provide any extra power

""""Algebric data types  (like Bool)
can be declared as follow
`data Compass = North | East | South | West`

Typing North in ghci triggers an error not a syntax error but an error related top the fact that the data type has no instance (a function) that allows it to be displayed in the compiler

we fix that :
```
instance Show Compass where
  show North = "North"
  show South = "south"
  show East = "East"
  show North = "West"
```
Etc for all the other relevant instances for your data such as Eq ...
This is no wonder a tedious process. Fortunatly that can be fixed with "deriving" as follow

```
data Compass = North | East | South | West
  deriving (Eq, Ord, Enum, Show)
```

These variables are sum types we also have product types that refer to tyeps where constructures takes additional parameters: like the maybe type:
`data Maybe a = Nothing | Just a `


### pattern matching
To manipulate lists we usually use head and tail: head will return the first element and tail will return the list except the first element

Lets see how to create ower own functions

```
newHead :: [a] -> a
newHead [] = error "empty list"
newHead (x:xs) = x

newTail :: [a] -> [a]
newTail [] = error "empty list"
newTail (x:xs) = xs
```

## Words "Project"  (https://github.com/Assassinkin/wordsGridGame)

To create a project with stack run:
`stack new words new-template -p "author-email:iuns@outlook.fr" -p "author-name:KaliAWSfatE" -p "category:dev" -p "copyright:BSD" -p "github-username:assassinKin"`
To compile it use : `cd words; stack ghci`

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

To build the project run `stack build` then run `stack exec words-exe` to execute the binary
we can change the project metadata, executable name, ... in the words.cabal file
we can add -dynamic in the `ghc-options` in the words.cabal file to make sure the binary size is reasonable
to run tests: stack test

when importing modules we need to specify that in the words.cabal file: add it to the `build-depends` section
If the module is needed for testing make sure to import it under `build-depends` for `test-suite`

in the test file: `$` in `main = hspec $ do` is a syntaxic sugar that replaces ()

Hoogle is a usefull haskell api search engine

if we need a function that have a specifuc type def we can search for it via hoogle
we need just to type [String] -> String

we can access a position i a list via `[1 ..] !! 50`
we can get a list of the 10 first value in a list via `take 10 [1 ..]`
also this is usefull: `takeWhile (10) [0 ..]`

**Monad**

List Monad notation
```mapped = do
  i <- [0 ..]
  return (i * 2)
```
-> return a list of [0 2 4 ..]
```filtered = do
   i <- [0 ..]
   guard (div2 i)
   return i
```
look into guard more
--> return a list of [ 0 2 4 ..]
we can combine mapping and filtering together as follow
```
pammedAndFiltered = do
  i <- [0 ..]
  guard (div2 i)
  return (i + 1)
```
--> return [ 1 3 5 ..]
we can also do that as follow :
` [ i * 2 | i <- [0..9], div2 i]` notice the guard expression after the ','
