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
