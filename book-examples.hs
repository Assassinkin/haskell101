-- These are notes from the book learn you a haskell

-- Int stands for integer.
-- Integer stands for, er … also integer. The main difference is that it's not bounded so it can be used to represent really really big numbers. I mean like really big. Int, however, is more efficient.

factorial :: Integer -> Integer  
factorial n = product [1..n] 

--ghci> :t head  
--head :: [a] -> a 
-- a: is a type variable.  Functions that have type variables are called polymorphic functions. The type declaration of head states that it takes a list of any type and returns one element of that type.


--Eq is used for types that support equality testing. The functions its members implement are == and /=. So if there's an Eq class constraint for a type variable in a function, it uses == or /= somewhere inside its definition. All the types we mentioned previously except for functions are part of Eq, so they can be tested for equality.

--ghci> 5 == 5  
--True  
--ghci> 5 /= 5  
--False 

-- Ord is for types that have an ordering.
--Members of Show can be presented as strings. All types covered so far except for functions are a part of Show. The most used function that deals with the Show typeclass is show. It takes a value whose type is a member of Show and presents it to us as a string.
--Read is sort of the opposite typeclass of Show. The read function takes a string and returns a type which is a member of Read.


-- ghci> read "4" fails 
--we need to explicitly tell the compiler what type we want in return.
--That's why we can use explicit type annotations. Type annotations are a way of explicitly saying what the type of an expression should be. We do that by adding :: at the end of the expression and then specifying a type. Observe:
--ghci> read "5" :: Int  
--5

--Bounded members have an upper and a lower bound.
--ghci> minBound :: Int 
