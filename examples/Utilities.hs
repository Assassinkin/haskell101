module Utilities (argmax, replace) where

-- Returns the elemnt of a list that maximizes f
argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f = foldl1 (\acc x -> if f x > f acc then x else acc)

-- Replace the ith elemnt of a list 
replace :: Int -> a -> [a] -> [a]
replace i item ls = x ++ (item:ys) where (x, _:ys) = splitAt i ls

