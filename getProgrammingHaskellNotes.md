
## Closures:
Closures are the logical consequence of having lambda functions and first-class functions.
By combining these lambda functions and first-class functions to create closures, you can
dynamically create functions.

If you find using closures confusing so far, you’re in luck! Thanks to partial application
parial function
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
