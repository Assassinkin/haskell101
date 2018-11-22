## Types

Types are set of values: The type integers includes Values such as `3` and `-4`.

Typeclasses are set of types: The typeclass `Num` includes types such as `Integer` and `Double`

A string is a list of Char `[Char]`

- use `:type` (`:t`) to get the type of a variable
- use `:info`  to get info on some variable type (Int Float etc ...)
- use `:help` for the full list

when you run :t on the `head` fuction we get:

```
ghci> :t head
head :: [a] -> a
```

`a`: is a type variable.  Functions that have type variables are called polymorphic functions. The type declaration of head states that it takes a list of any type and returns one element of that type.

`:t 3`: `3 :: Num a => a` a is a general type in the Num typeclass. a is called type variable

3 is part of integer double etc. it is not committing to a specific type cause it is lazy hhhh. To force it to commit to a type we need to bind it to a variable
```
let x = 3
:t x   -- now output x::Integer
```

We can also force it using type annotation: `3 :: Double`.

Function also have types

`:t (+)`: we can also ask for the type of the + operator but when doing so we need to use ().

Int stands for integer. Integer also stands for integer. The main difference is that it's not bounded so it can be used to represent really really big numbers. I mean like really big. Int, however, is more efficient.
```
factorial :: Integer -> Integer
factorial n = product [1..n]
```

`Eq` is used for types that support equality testing. The functions its members implement are == and /=. So if there's an Eq class constraint for a type variable in a function, it uses == or /= somewhere inside its definition. All the types we mentioned previously except for functions are part of Eq, so they can be tested for equality.

```
ghci> 5 == 5
True
ghci> 5 /= 5
False
```
to force haskell to treat a var in a certain way use: `(123 :: Float)` then 123 will be considered float.

`Ord` is for types that have an ordering.

The most used function that deals with the Show typeclass is `show`. It takes a value whose type is a member of Show and presents it to us as a string.

Read is sort of the opposite typeclass of Show. The `read` function takes a string and returns a type which is a member of Read.

`ghci> read "4"` fails as we need to explicitly tell the compiler what type we want in return. That's why we can use explicit type annotations. Type annotations are a way of explicitly saying what the type of an expression should be. We do that by adding :: at the end of the expression and then specifying a type: `ghci> read "5" :: Int`

To load modules into the prompt: `:m + module name`. For example: `:m + Data.List`


## List manipulation

A list is an empty list "[]" or a variable appended to another list

`1: [2]` outputs `[1, 2]`

To construct a list we basically use `5 : []` (output is [5])

`let numbers = [1,2,5,6,9,8,5,2,41,4,522]`

`head` and `tail` operations are constant time.

`reverse numbers` This operation is linear time as it requires creating new nodes.

Access to a position in a list can be done via `[1 ..] !! 50`.

We can get a list of the 10 first value in a list via `take 10 [1 ..]`.

Also this is useful: `takeWhile (10) [0 ..]`.

`last` give us the last element.

`init` gives us everything but the last element and both take linear time.

`null numbers`: false if the list is not empty and takes constant time.

`elem 15 numbers`: (==> true) Tells us if a particular element is in the list: linear time.

`[1,5,1] ++ [2,5,3]`: take linear time.

`"adam" < "ant"` :(==> true) It compares letter by letter using asci till it finds something different as d < n

*strings are list of caracters*  *list of lists are there 2*

`maximum numbers` `minimum numbers`

`sum numbers`: sum of all the element in the list.

`product numbers`: Product of all the element in the list.

==> take all linear time

sum [1..100]

`[2,5..10]` => [2,4,6,8,10]

`[10,9..1]` => form 10 to 1 by 1

`[1..]` => from 1 to infinite

```haskell
let list = [1, 20 ..]
take 10 list -- take 10 element from the 'infinite' list
```

`[2^n | n <- [1..10]]`: A list of 2^n where n in that list. so basically 2 4 8 16 32 64 ...

_we can also add predict or filters placing other constraints_

[2^n | n <- [1..10], 2^n >= 10 , 2^n < 100 ]

We can use it with strings too. lets remove all the voyels from a string

`[x | x <- "outrageous", not(elem x "aeiou")]`. We can use the backtics 2 here `not( x `elem` "aeiou")`.

## Tuples

If at the time of writing the code (compile-time) you know that a particular function will always return two values, you can encode that guarantee by picking the right type ie a tuple of x element (x-tuple)

The size of a list is not part of its type, tuple is

`[1,2] == [1,5,2]`: ==> false

`(1,2) == (1,5,3)`: error, it doesnt make sense

The elements of a list can't be of a different types, tuple can

Handy functions:
* fst first element of a tuple of 2.
* snd second element of a tuple of 2.
* zip turns 2 lists into a list of pairs.

 This

 ```haskell
[ "a", ["Adam", "Anurag", "Ashtong"]
, "b", ["Bhushan", "Bimal", "Banksky", "Beth"
, "c", ["Charlie", "Chaman", "Chilgoza"]
]
```
Won't work in haskell cause all elements in a list should be of the same type. However, in the data-structure given above, the list elements are alternating between `String` and `[String]`.
best way for this to work is using a list of 2-tuple:

```haskell
[ ('a', ["Adam", "Anurag", "Ashtong"])
, ('b', ["Bhushan", "Bimal", "Banksky", "Beth"])
, ('c', ["Charlie", "Chaman", "Chilgoza"])
]
```

## let and case expressions

let expression:
```haskell
let pattern = result
         ...
         in result
```
-> can be used anywhere and can be nested

case expression:
***[WIP]***

## Functions

```
identifyCamel humps = if humps == 1
                       then "dromedary"
                       else "Bactrian"
```

- Load function in the command line interpreter with :load programm or :l programm

- The else part is needed. a haskell function always return something


```haskell
noVowels :: [Char] -> [Char]  -- here we can define our function type
noVowels word = if word == ""
                then ""
                else if head word `elem` "aeiouAEUOI"
                  then noVowels (tail word)
                  else (head word) : noVowels (tail word)
```
To run it: `noVowels "here is my string that will get stripped off its vowels"`

this is (x:xs) haskell pattern matching syntax and it is quite usefull

Here the same func with pattern matching:
```haskell
noVowels "" = ""
noVowels (x:xs) = if x `elem` "aeiouAEUOI"
                  then noVowels xs
                  else x : noVowels xs
```
If needed we can use `noVowels _ = "no match"` if we need to put something to match everything.

We also use guards to specify conditions and what we should do in each case
```haskell
noVowels "" = ""
noVowels (x:xs)
         | x `elem` "aeiouAEUOI" = noVowels xs
         | otherwise             = x : noVowels xs
```

`show n ++ " blabla"`: `Show n ++` is used to concatenate a number `n` with a string. it basically transform a number to a string

It is better to define variable in scope and as locally as possible.

Here a function to calculate the gravity of earth and notice that we use the gravitational constant inside the function
```haskell
gravity :: (Fractional a) => a -> a
gravity r = let g = 6.674e-11
                earthmass = 5.972e24
            in g * earthmass / (r ^ 2)
```
To have multiple function in a file just write them up under each other with no need for module key word.

### recursive functions

```haskell
listEvenSum :: Int -> [Int] -> Int
listEvenSum total list =
  if (list == [])
    then total
    else if (mod (head list) 2) == 0
      then listEvenSum (total + (head list)) (tail list)
      else listEvenSum total (tail list)
```

will keep iterating over the list and adding the head to the total until the list is empty then we can return the total (recursive stuff)

### lambda functions

Allows to define function on the fly
`map (\x -> x+1) [1..5]` -> [2,3,4,5,6]

`\` is backslash cause it is similar to lambda

## Higher order functions

f :: (Int -> Int) -> Int -- f takes a funtion and return an Int

g :: Int -> (Int -> Int) -- g takes an Int and return a function

### map

This higher-order function abstracts out the procedure of going through every element in a list and transforming it to a new value. That’s it. You cannot change the order of elements. You cannot skip elements. You cannot add elements. The lambda that you pass, will be run on every single element of the list and the output values will be collected in a list and returned. Which means, that the length of the input list and output list will always be the same.

`map` basically is a useful high order function. It takes a function and applies it to each element of a list.

- `map add1 [1,5,2,3]`: map apply the function add to each element of the list.
- `map (max 3 ) [1,2,3,4,8,6]` -> `[3,3,3,4,8,6]`
- `map (10/) [1,2,5,10]` -> [10.0,5.0,2.0,1.0

type signature:
```haskell
GHCi> :t map
map :: (a -> b) -> [a] -> [b]
```
This is what is called a polymorphic function in Haskell. Any type that is represented by lowercase identifiers in the type-signature, like a, b, server, file, instead of Server or File, will work if you pass values of any type to it. However, these “type-variables” will specify a relationship between the various arguments of the function.

**note**: Whenever you see parentheses in a type-signature, remember that you need to pass a function/lambda of that shape/type to that argument.

Next is something also cool but watch the [video](https://www.youtube.com/watch?v=XKUsGSjnITc&list=PLS6urCrsYES24Fwzg5-Uga1QEbNm9kiU_&index=10) to understand it better.

```haskell
h :: Int -> Int -> Int
h x y = x+y
```
`h` is basically `h :: Int -> (Int -> Int)`

`h x y` is basically `(h x) y` . Watch the video for better understanding

### filter

Filter allows us to select elements, which satisy a given condition, from a list. It is abstracts away the repetitive procedure of going through every element in an input list, checking each element against a condition, and collecting all elements which pass that condition into a new list.

`filter (>5) [1..10]` -> `[6,7,8,9,10]`

### foldl' and foldl (and foldr)

- `foldl (+) 0 [5,3,6]`: equivalent to ((0+5)+3)+6
- `foldl (\*) 1 [5,3,6]`: for the multiplication
- `foldr (+) 0 [5,3,6]`: foldr same but from right ==> 5+(3+(6+0))

The strangely named foldl' is an extremely important higher-order function. In some senses, it is the “mother function” of all higher-order functions that deal with processing lists. Using foldl' you can implement, map, filter, and every other such function that visits each element and do something with it.

If you need to access the result of the element already processed filter and map are no good.

fold will loop through each element of the list (x) and sum it with total which starts with 0 in general and keep adding elements from the list
`foldl' (\total x -> total + x) 0 [1, 2, 3, 4]`

### zipWith and others

`zipWith (+) [1,2,5,8] [2,5,5,5]` -> `[3,7,10,13]`

`from = flip (-)`
example : ```5 `from` 7``` -> 2

`takeWhile (<10) [1,3..]` -> `[1,3,5,7,9]`


## Download Haskell Stack

wget -qO- https://get.haskellstack.org/ | sh

`stack` is similar to nvm or yarn from the NodeJS world

It is good for the following:
- Having different versions of the Haskell compiler (i.e. ghc) available on your machine without messing things up, and using the right ghc version for your project.
- Taking care of which Haskell libraries are known to compile/build with which version of ghc.
- Taking care of the dependency graph of libraries, so that all the libraries that your project depends on, compile successfully without you having to manually specify the version of each library. Basically stack saves you from the dependency hell problem.

## Hackage vs Stackage & Cabal vs Stack

Hackage is the original package repository. This is where authors upload their packages to share them with the world.
Stackage pulls specific versions of specific packages from Hackage and divides them into “sets” known as “snapshots”. Each snapshot contains only a single version of any package with a guarantee that all the packages in a given snapshot will successfully build when included in a project. That is, you will not get a dependency hell when your project depends on 5 packages from the same Stackage snapshot. (If you go to a snapshot/LTS’ listing page you can verify that there is only one version of each package listed there. On the other hand, if you check the same package on Hackage, you will see all versions of the package listed there).
Hackage has way more packages than Stackage, because, not every author adds their package to the Stackage snapshot. This is probably because, every time a new LTS/snapshot is released, package-authors have to do some extra work to maintain the “no dependeny-hell guaranteee”. However, most popular/important packages have already been added to Stackage, so you won’t be missing any packages till you start doing advanced Haskell stuff.
The command-line tool called cabal does not know about Stackage and pulls packages directly from Hackage. Also, cabal+Hackage do not give this “no dependency-hell guarantee” that stack+Stackage works very hard to maintain.
The command-line tool called stack pulls packages from Stackage, by default. Having said that, it can pull packages from Hackage if a particular package is not available in its snapshot (but this requires a few extra lines of config).



## No “truthy” or “falsy” values

Unlike other languages, Haskell doesn’t have the concept of truthy and falsy values. Some other languages treat a number of non-boolean values, like 0, 1, any string, empty array, empty object, etc, as either a boolean True or a boolean False. Haskell doesn’t do that. Haskell has only two boolean values - True or False. This is a result of the strong & principled type-system of Haskell.

## Data Structure

Type synonym are useful for readability but they don't provide any extra power

### Algebraic Data Types [WIP]

Algebric data types  (like Bool)
can be declared as follow
`data Compass = North | East | South | West`

Typing North in ghci triggers an error not a syntax error but an error related top the fact that the data type has no instance (a function) that allows it to be displayed in the compiler

we fix that :
```haskell
instance Show Compass where
  show North = "North"
  show South = "south"
  show East = "East"
  show North = "West"
```
Etc for all the other relevant instances for your data such as Eq ...

This is no wonder a tedious process. Fortunatly that can be fixed with "deriving" as follow

```haskell
data Compass = North | East | South | West
  deriving (Eq, Ord, Enum, Show)
```

These variables are sum types we also have product types that refer to types where constructors takes additional parameters: like the maybe type: `data Maybe a = Nothing | Just a `


## Monad: [ Heavy WIP] below is basically blabla

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
