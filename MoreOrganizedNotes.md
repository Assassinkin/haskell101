
## Closures:
Closures are the logical consequence of having lambda functions and first-class functions.
By combining these lambda functions and first-class functions to create closures, you can
dynamically create functions.

If closures are confusing then use partial functions
```haskell
add4 a b c d = a + b + c +d
partiFunc = add4 5
```

## Lists:

When taking apart a list, the main pieces are the head, the tail, and the end (represented
by []). The head is just the first element in a list. The tail is the rest of the list left over, after the head

The tail of a list with just one element is [], which marks the end of the list.

By definition, a list is always a value consed with another list (which can also be an empty list).

To build a list, you need just one function and the infix operator (:), which is called cons.

Every element of the list must be the same type, For example, you can cons the letter 'h' to the string "ello" because "ello" is just a list of characters and 'h' (single quotes) is a character: `'h':"ello"` -> "hello"

But you can’t cons "h" (double quotes) to "ello" because "h" is a list of one character and the values inside "ello" are individual characters.

Consing is important to understand because it’s an essential part of writing recursive
functions on lists. Nearly all sequential operations in functional programing involve
building lists, breaking them apart, or a combination of the two.

### Common function on list:
Because lists are so important, a wide range of useful functions are built into Haskell’s standard library module, called Prelude. So far, you’ve seen head, tail, : and ++, which allow you to take apart lists and put them back together. There are many other useful functions on lists that will come up so frequently when writing Haskell that it’s worth familiarizing yourself with them.

- `head`, `tail`, `++`

- The `!!` operator: access a partticular element of the list at a certain location

- The `length` function gives you the length of the list!

- `reverse` reverses the list

- The `elem` function takes a value and a list and checks whether the value is in the list

_`elem` is a function that you may want to treat as an infix operator for readability. Any binary function can be treated as an infix operator by wrapping it in back-quotes (`)._

- The `take` function takes a number and a list as arguments and then returns the first n elements
of the list

Take the last n element of a list:

`takeLast n aList = reverse (take n (reverse aList))`
->
```haskell
GHCi> takeLast 10 [1..100]
[91,92,93,94,95,96,97,98,99,100]
```
=> we need to run reverse again to make sure we do not get the last items in a reversed order

- `drop` is similar to take, except it removes the first n elements of a list

- You use `zip` when you want to combine two lists into tuple pairs. The arguments to zip are two lists. If one list happens to be longer, zip will stop whenever one of the two lists is empty:
```haskell
GHCi> zip ['a' .. 'f'] [1 .. ]
[('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
```

- The `cycle` function is particularly interesting, because it uses lazy evaluation to create an infinite list. Given a list, cycle repeats that list endlessly.
`ones n = take n (cycle [1])`

```haskell
GHCi> ones 4
[1,1,1,1]
```

## Recursion and pattern matching

There is no classic loops( for, while and until) in haskell as they rely on changing states
-- > using recursion

Haskell provides a feature called pattern matching to make recursion much saner to reason about.

A popular convention in Haskell is to use the single x to represent a single value, and the variable xs to represent a list of values.

we can use `error` to throw an error if we can't handle a given case: in head for example:
`myHead [] = error "No head for empty list"`

##  higher-order function

A higher-order function is technically any function that takes another function as an argument.

The `map` function takes another function and a list as arguments and applies that function to each element in the list

The `filter` function behave similarly to `map` but works by keeping only the elements of the list that pass the test

`remove` removes the items that pass the test in a list

The function `foldl` (the l stands for left, which we’ll explain soon) takes a list and reduces it to a single value. The function takes three arguments: a binary function, an initial value, and a list. The most common use of foldl is to sum a list

`foldl` and `map` can be used together in a great way  such as the sum or squares: `sumOfSquares x = foldl (+) 0 (map (^2) x)`

The alterative to `foldl` is `foldr` The reason we call it a right fold is that there are two arguments in a binary function: a left argument and a right argument. The left fold compacts the list into the left argument, and the right fold into the right argument.

- `foldl` is the most intuitive behaving of the folds, but it usually has terrible performance
and can’t be used on infinite lists.
- `foldl'` is a nonlazy version of foldl that’s often much more efficient.
- `foldr` is often more efficient than foldl and is the only fold that works on infinite
lists.

## Functional OOP

The common approach of calling methods. When calling methods, your object > action: `car.start()` (car is the object, and start is the action). functional approach will invert this pattern by sending a message to an object: `start car`

#### build fighting robots!

The object modeling the robot will have some basic properties:
- A name
- An attack strength
- A number of hit points

A robot constructor: `robot (name,attack,hp) = \message -> message (name,attack,hp)`

Create an instance of your robot like this: `nagatoPain = robot ("Pain",25,200)`

To make this object useful, you’ll have to add a few accessors so you can work with these values more easily.

name, attack, and hp helper functions:
```haskell
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp
```

With these helper functions, we can easily implement getters.
```haskell
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp
```

Setters will allow to set the properties. Each of these cases will have to return a new
instances of the robot.

setName, setAttack, and setHP accessors:
```haskell
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))
```

setters work as follow by basically creating a new robot in our case which we do clone after the basic one:
`nicerRobot = setName killerRobot "kitty"`

Defining a printRobot message:
```haskell
printRobot aRobot = aRobot (\(n,a,h) -> n ++
" attack:" ++ (show a) ++
" hp:"++ (show h))
```
then we can run it like this: `printRobot killerRobot`

Haskell isn’t an object-oriented language. All of the functionality built here from scratch already exists in a much more powerful form, using Haskell’s type system.  Haskell’s types can replicate all the behavior  needed for OOP.


# Types

Python, and JavaScript languages uses `dynamic typing`, decision concerning variables are taken during runtime.

- dynamic typing (+): more flexibility, no need to keep track of types
- dynamic typing (-):  errors happen only at runtime.

`static typing` used by languages such as C C++ and Java

- static typing (+): types issues occurs during compile time
- static typing (-): Add many annotation for types for each variable and function.

Haskell is a statically typed programming language. Haskell make use of type inference as it can helpfigure the type of a variable on its own.

-> It’s Haskell’s type system that sets it apart from other programming languages.  

                                      Variable name :: Variable type
                    Type signature:      something :: Int
                    Variable definition: something = 20

Types start with a capital letter to distinguish them from functions (lowercase letter or \_).

Int is bounded while Integer is not.

A list of character is a string. String as a type synonym for [Char].

A more useful type is Tuple which is different then list. Something to note is that a tuple of char is just one character and can't be a string.

`->` is used to separate arguments and return values when defining a function type.

The read function works by taking a string and converting it to another type.
`x = read "8"` to what type we can assign x ??

we can do this: `x= read "8" :: Int`

Always the last type if a function is the type if the return value.

      type of arg1 -> type of arg2 - > type of return value
      funct :: Int -> String -> String -> (Int, String)

Types for first-class functions: Put the function type signature in parenthesis.

        ```haskell
        ifEven :: (Int -> Int) -> Int -> Int
        ifEven f n = if even n
          then f n
          else n
        ```

Use type variables to make sure a function can apply to all Types instead of creating a func for each type. Using type variabels to implement simple as =follow:
```haskell
simple :: a -> a
simple x = x
```

We use different letter to indicate that we might use different types.

func1 :: a -> a
func2 :: a -> b

-> this means that func1 will produce a result with a type similar to the input whereas func2 can produce a wider range of output types

## Type synonym

It is a good practice to use type synonym to rename the existing types when used in a given function to mak ethe code easier to understand:

                        density :: Double -> Double -> Double

This is an OK type signature, but this swill obviously be better:

                        density :: Mass -> Volume -> Density

To create a type synonym use `type`

```haskell
type Mass = Double
type Volume = Double
```

We can use type synonym as follow 2: ```type PersonName = (String,String)```

## Creating new types

To create new types use `data`.

Sometime it is better to create a new type instead of trying to force a type to our use case.

`data Sex = Male | Female`: sex type is an either instance of these data constructors. This is so similar to `data Bool = True | False`

To create a type for blood type we need to take care of the ABO and the (positive/negative) thing
Creating something loke data BloodType = A+|A-|O+|O- ... is retarted
It would be better to use a data type for positive/negative and another for A/B/O things then combine them.

```haskell
data ABOType = A | B | AB | O
data RhType = Pos | Neg
data BloodType = BloodType ABOType RhType`
```
-> Second BloodType is the data constructor:  A `data constructor` is used to create a concrete instance of the type.

-> Now we cna create person's blood type:
```haskell
person1 :: BloodType
person1 = BloodType O Neg
person2 :: BloodType
person2 = BloodType AB Pos
person3 :: BloodType
person3 = BloodType O Pos
```

When representing a data type sometimes it get tedious with large representation.
representing a person for example can be done via:
`data Person = Person Name Sex Int Int Int BloodType`. This can be ambiguous sometimes to understand which way Haskell provides `record syntax`
```Haskell
data Person = Person { name :: Name
 , sex :: Sex
 , age :: Int
 , height :: Int
 , weight :: Int
 , bloodType :: BloodType }
```
With `record syntax` we can easily define data for our type and we can set each field by name, the order matter no more.
```Haskell
dovahKin :: Person
dovahKin = Person {name = Name "Dovah" "Kin"
, age = 20
, sex = Male
, height = 180
, weight = 70
, bloodType = BloodType O Pos }
```
Furthermore `record syntax` provides an easy way to access each field (no need for getters)
```Haskell
GHCi> height dovahKin
62
```

Changing value can be done as follow (we need to assign that to a new person as we are pure here)

                    dovahAssassin = dovahKin { age = 22 }

## Type classes

Type classes allow you to group types based on shared behavior.

`Num` is a type class generalizing the idea of a number. All things of class `Num` must have a function `(+)` and `(-)` defined on them.

We can use `:info` to get all the functions in a type class.
```Haskell
GHCi> :info Num
class Num a where
 (+) :: a -> a -> a
 (-) :: a -> a -> a
 (*) :: a -> a -> a
 negate :: a -> a
 abs :: a -> a
 signum :: a -> a
```
- `:info` shows the definition of the type class.

The definition is a list of functions that all members of the class must implement, along with the type signatures of those functions. The family of functions that describe a number is +, -, \*, negate, abs, and signum. Each type signature shows the same type variable `a` for all arguments and the output. None of these functions can return a different type than it takes as an argument.


### Defining a type class:

We need to name the type class (typeName) and define the signature of each of the functions affiliated to the type class.

                        ```Haskell
                        class TypeName a where
                          fun1 :: a -> a
                          fun2 :: a -> String
                          fun3 :: a -> a -> Bool
                        ```
### Common type classes

*Ord*
for the greater oepration: `>`:
```Haskell
GHCi> :t (>)
(>) :: Ord a => a -> a -> Bool
```
getting the type of the func > reveals that it is part of the `Ord` type class. `Ord` provide functions that compare all possible types (strings Int Double ...)
