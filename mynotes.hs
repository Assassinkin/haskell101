
hypotenuse a b = sqrt (a^2 + b^2)
-- we need the () for precedence reason not cause sqrt is a function
-- can't be in with a capital letter.

-- 2nd function
identifyCamel humps = if humps == 1
                         then "dromedary"
                         else "Bactrian"

-- load function in the command line interpreter with :load programm or :l programm
-- the else part is needed. a haskell function always return something

-- let x = 3
-- let y = 4
-- hypotenuse x y
-- this is fail cause x and y are integers
-- :quit or :q to quit

let numbers = [4,5,4,8,15,24]
-- head numbers
-- tail numbers
-- to construct a list we basically use 5 : [] (output is [5])
-- 1 : 5 : []
                -- head and tail operations are constant time
reverse numbers -- this operation is linear time as it requires creating new nodes

-- index or the !! operator give us the the element of at a certain index.
numbers !! 3
-- output 8 (cause 0 1 2 3 ) (take linear time as we need to walk down the list)
-- last give us the last element.
-- init gives us everything but the last element and both take linear time
null numbers
-- false if the list is not empty and takes constant time
elem 15 numbers
-- ==> true
-- tells us if a particular elemnt is in the list -- linear time
[1,5,1] ++ [2,5,3]
--take linear time
"adam" < "ant"
-- ==> true
-- it compares letter by letter using asci till it finds something different as d < n
-- strings are list of caracters
-- list of lists are there 2

maximum numbers
minimum numbers
sum numbers     -- sum of all thge elemnt in the list
product numbers -- product of all the element in the list
-- take all linear time
sum [1..100]
-- works fir caracters 2
[2,5..10]  -- [2,4,6,8,10]
[10,9..1] -- form 10 to 1 by 1
[1..] -- from 1 to infinite

-- list comprehension
[2^n | n <- [1..10]]
-- a list of 2^n where n in that list. so basically 2 4 8 16 32 64 ...
-- we can also add predict or filters placing other constraints
[2^n | n <- [1..10], 2^n >= 10 , 2^n < 100 ]

-- we can use it with strings too. lets remove all the voyels from a string
[x | x <- "outrageous", not(elem x "aeiou")]
-- we can use the backtic 2 here not( x `elem` "aeiou")
-- ==> "trgs"

-- tuples
-- the size of a list is not part of its type, tuple is
[1,2] == [1,5,2]
-- ==> false
(1,2) == (1,5,3)
-- error, it doesnt make sense

-- the elements of a list can't be of a different types, tuple can
-- handy functions : fst first element of a tuple of 2
--                   snd second element of a tuple of 2
--                   zip turns 2 lists into a list of pairs


-- types are set of values , typeclasses are set of types
-- the type integers includes calues such as 3 and -4
-- the typeclass Num includes types such as Integer and Double
:t True
-- output True :: Bool
-- :t is used to find the type of a value
-- a string is a list of Char [Char]

:t 3
-- ==> 3 :: Num a => a   a is a general type in the Num typeclass. a is called typevariable
-- 3 is part of integer double etc. it is not commiting to a specific type cause it is lazy hhhh
-- to force it to commit to a type we need to bind it to a variable
let x = 3
:t x   -- now output x::Integer
-- we can also force it using type annotation
3 :: Double
-- function also have types
:t (+) -- we can also ask for the type of the + operator but when doing so we need to use ()

-- Functions
noVowels :: [Char] -> [Char]  -- here we can define our function type
noVowels word = if word == ""
                then ""
                else if head word `elem` "aeiouAEUOI"
                  then noVowels (tail word)
                  else (head word) : noVowels (tail word)
-- to run use
noVowels "here is my string that will get stripped off its vowels"
-- this is (x:xs) haskell pattern matching syntax and it is quite usefull

-- here the same func with pattern matching
noVowels "" = ""
noVowels (x:xs) = if x `elem` "aeiouAEUOI"
                  then noVowels xs
                  else x : noVowels xs
-- if needed we can use noVowels _ = "no match" if we need to put something to match everything


-- we also use guards to specify conditions and what we should do in each case
noVowels "" = ""
noVowels (x:xs)
         | x `elem` "aeiouAEUOI" = noVowels xs
         | otherwise             = x : noVowels xs

show n ++ " something something"
-- show n ++ is used to concatenate a number "n" with a string. it basically transform an number to a string

-- it is better to define cariable in scope and as locally as possible
-- here a function to calculate the gravity of earth
gravity :: (Fractional a) => a -> a
gravity r = let g = 6.674e-11
                earthmass = 5.972e24
            in g * earthmass / (r ^ 2)

-- templates :
-- a definition : pattern = result
--                ...
-- the pattern should be exhaustive so no case should slip through
-- it is a good idea to have the last pattern match everything

 -- a guard expression : pattern
 --                        | expression = result
 --                        ...
 --                        | otherwise = result

 -- the otherwise is not required but it is a good idea to keep the rule exhaustive
-- a where clause : result where
--                  pattern = result
--                  ...
-- only used inside a definition and can't be nested

-- le expression : let pattern = result
--                     ...
--                 in result
-- can be used anywhere and can be nested

-- case expression : case expression of pattern -> result
--                                      ...
-- !!!!!!!!!!!!!!!!! indentation is important !!!!!!!!!!!!!!!!!
-- make sure to use space instead of tabs to mitigate indefined behaviours

-- Take a look into the tictac game
-- Hgher order functions:
f :: (Int -> Int) -> Int -- f takes a funtion and return an Int
g :: Int -> (Int -> Int) -- g takes an Int and return a function


-- map is a usefull high order function
map add1 [1,5,2,3] -- map apply the function add to each element of the list
map  (max 3 ) [1,2,3,4,8,6]
-- => [3,3,3,4,8,6]
-- we can even use thing like this :
map (10/) [1,5,3,6]
-- pretty cool I'd say

-- Next is something also cool but watch the video to understand it better : https://www.youtube.com/watch?v=XKUsGSjnITc&list=PLS6urCrsYES24Fwzg5-Uga1QEbNm9kiU_&index=10
h :: Int -> Int -> Int
h x y = x+y
-- h is basically h :: Int -> (Int -> Int)
-- h x y is basically (h x) y . as i said watch the video for better insights

zipWith (+) [1,2,5,8] [2,5,5,5]
-- => [3,7,10,13]
from = flip (-)
5 `from` 7 -- => 2
filter (>5) [1..10]
-- => [6,7,8,9,10]
takeWhile (<10) [1,3..]
-- => [1,3,5,7,9]

-- Lambda Functions
-- allows to define function on the fly
map (\x -> x+1) [1..5]
-- the lambda function is between the parenthesis
-- \ is backslash cause it is similar to lambda
foldl (+) 0 [5,3,6] -- equivalent to ((0+5)+3)+6
foldl (*) 1 [5,3,6] -- for the multiplication
foldr (+) 0 [5,3,6] -- foldr same but from right ==> 5+(3+(6+0))


-- Haskell functions can be grouped into modules (class and a package in java)
-- To load modules into the prompt: :m + module name
:m + Data.List
-- defining types will be covered in the card.hs
-- Defining typeclasees
data Quad a = Quad a a a a
instance (Show a) => Show (Quad a) where
    show (Quad a b c d) = (show a) ++ " " ++ (show b) ++ "\n" ++
                          (show c) ++ " " ++ (show d)
-- Quad is a 2 by 2 matrix
Quad 1 2 3 4
-- ==> 1 2
--     3 4
-- They did got printed like that cause we told it by making Quad an instance of the typeclass show
-- Here we are defining our own typeclass
class Floppable a where
  flop :: a -> a
instance Floppable (Quad a) where
  flop (Quad a b c d) = Quad a c b d
-- here we making a Quad instance of the typeclass Floppable . we making sure it transpose the matrix

-- input & output: Check hangman.hs

-- What to do next ?
-- need to learn more on functors, types, classtypes and the typeclass instance thing
