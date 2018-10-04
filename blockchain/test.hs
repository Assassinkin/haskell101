checkNumber :: Int -> Int
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

-- will keep etirating over the list and adding the head to the total until the list is empty then we can return the total (recursive stuff)




-- high order function∷
--import Data.Char (toUpper)
--map (\x -> toUpper x) "abc"

-- fold will loop through each element of the list (x) and sum it with total which starts with 0 in general and keep adding elements from the list
--  foldl' (\total x -> total + x) 0 [1, 2, 3, 4]

-- check if the result hold that number if so it is ignored else it is added
-- GHCi> foldl' (\result x -> if (x `elem` result) then result else (x:result)) [] [1, 2, 3, 3, 2, 3, 1]


-- 5 `elem` [1,5,3]


-- next is tuple
