main :: IO ()

main = putStrLn (greet "World")
-- we can c=run the main functions or/and any other function from within the file
-- we can use :edit to edit the file from whithin a text editor (vi in my case)
greeting = "Hello"
greet who = greeting ++ ", " ++ who

add Int -> Int -> Int
add = (+)
-- even though (+) can add floating number and stuff add can't cause we restricted its type to Int
