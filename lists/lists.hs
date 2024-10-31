

main :: IO ()

-- lists -> collection of values of same type,
myList::[Int]
myList = [1..5]

firstElement,lastElement :: Int
restElements,allButLast :: [Int]

firstElement = head myList      -- 1
restElements = tail myList      -- [2, 3, 4, 5]
lastElement = last myList       -- 5
allButLast = init myList        -- [1, 2, 3, 4]
-- isEmpty :: Bool
isEmpty = null myList

-- we can add elements to a list in a couple of ways:
-- Using : (Cons Operator): Adds an element to the front of the list.
-- Using ++ (Concatenation Operator): Combines two lists.

myList2 = 0 : myList

myList3 = [7,8,9]

combinedList = myList2 ++ myList3

listLength = length myList      -- 5
containsTwo = 2 `elem` myList   -- True

-- main = do
--     print (firstElement,lastElement,restElements,allButLast)
--     print isEmpty
--     print combinedList
--     print listLength
--     print containsTwo


-- list comprehensions..
-- syntax: 
-- functions name = [expression | element <- iterable, condition]
-- Create a list of squares of even numbers from 1 to 10
-- evenSqr :: [Int]
-- evenSqr = [x*x | x <-[1..10], even x]
-- evenSqr = [x|x<-[1..10],even x]
-- main = print evenSqr

-- funtction that doubles each element in the list

doubleNum :: [Int]
doubleNum = [x*2 |x <-[1..10]]
-- main = print doubleNum

-- Common List Functions
-- Here are some common built-in functions for lists:

-- map: Applies a function to each element of a list.
-- filter: Returns a list of elements that satisfy a predicate.
-- foldl and foldr: Reduce a list to a single value using a binary function.

increment :: [Int]
increment = map (+1) combinedList
-- main = print increment

evenList::[Int]
evenList = filter even combinedList
-- main = print evenList

vowelsFromString :: String -> String
vowelsFromString str = [c | c <- str, c `elem` "aeiouAEIOU"]
-- Example usage: 
-- main = print (vowelsFromString "Hello, World!") -- "eoo"

-- Use list comprehension to create a list of characters from a string that are vowels.
matrix :: [[Int]]
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened :: [Int]
flattened = [x | row <- matrix, x <- row]   -- [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- squaresOfEvens = [x * x | x <- [1..10], even x]   -- [4, 16, 36, 64, 100]




