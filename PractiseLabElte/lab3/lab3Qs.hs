{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}



main :: IO()


---------------------------------------------------------------------------------------
---- The f function will take a positive integer n and return the sum of (3i + 4)*(i + 5) for i from 1 to n .
-- If n is 0, it should return 0.
-- solution - 1
-- f :: Int -> Int
-- f n 
--     | n == 0    = 0  -- Base case: if n is 0 or negative, return 0
--     | otherwise = (3 * n + 4) * (n + 5) + f (n - 1)

-- solution 2 ( usin g lists)
f :: Int -> Int
f 0 = 0  -- Base case: if n is 0, return 0
f n = sum [(3 * i + 4) * (i + 5) | i <- [1..n]]

-- main = print (f 3) -- 216
-- main = print (f 1) -- 42
-- main = print (f 0) -- 0
-- main = print (f 6) -- 792

---------------------------------------------------------------------------------------
{-
        Overview of lists:
            1. Homogeneity: All elements of list must be of the same type.
            2. Basic functions:
                1. "head list" retrieves the first element.
                2. "tail list" returns all but the first element.
            3. Use "++" to combine two lists.
            4. "length list" gives the number of elements of list
            5. Nested Lists: Lists can contain other lists.
            6. Indexing: Access elements with (list !! index).

            for more information check: https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html


-}


--Generate 2 digits number starting with 9
-- main = print [90,91,92,93,94,95,96,97,98,99]
-- number :: [Int]
-- number = [x | x <- [90..99]]
-- main = print number

--Generate multiples of 5 in the range of 0 to 100
multiple5 :: [Int]
multiple5 = [x | x <- [0..100], x `mod`5 == 0]
-- main = print multiple5 
-- [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100]

-- Generate positive even numbers from 0 up to 100
positiveEven::[Int]
positiveEven = [x | x <- [0..100], even x]
-- main = print positiveEven
--[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]


--Generate 2 digits number starting with 9
numbers = [90..99]
--main = print ?? -- [90,91,92,93,94,95,96,97,98,99]

---------------------------------------------------------------------------------------
-- Basic list functions - review

-- main = print (head [101, 20, 33, 43, 51] )    -- 101 first element of the list

--main = print (head [ [101, 20], [33, 43, 51]] )  -- [101,20]

--main = print (head [[[101, 20]], [[33]],[[ 43, 51], [1,2,3]]] )  -- [[101,20]]

--main = print (tail [10, 22, 32, 43, 58] )  -- [22,32,43,58] everything except first element as a list

-- main = print (drop 5 [1, 2, 3, 4, 5, 6, 7] ) -- [6,7] delete first 5 elements

-- main = print (take 4 [1, 2, 3, 4, 5] ) -- [1,2,3,4] take first 4 elements

-- main = print ([1..10] ++ [8, 88] )  -- [1,2,3,4,5,6,7,8,9,10,8,88] concatenation, appends the second to the first

-- main = print (reverse [1..8])  -- [8,7,6,5,4,3,2,1] reverses a list

-- main = print (length [1..100] )   -- 100 number of elements 

--main = print (last [100, 200, 300])   -- 300 last list element

--main = print (init [100, 200, 300])  -- [100,200] all except the last          

-- main = print ( 2 `elem` [0..22] ) -- True, check membership 

--main = print ( elem 5 [10..20]) -- False

--main = print (concat [[1,2,3,4], [5], [6,7,8]] ) -- [1,2,3,4,5,6,7,8] flattens a list of lists

---------------------------------------------------------------------------------------
---- Define two lists. First one should include first 5 positive integers. 
-- Do this by writing them one by one.
-- Second one should include first 101 positive integers. Do this by using .. notation

first :: [Int]
first = [1,2,3,4,5]
second :: [Int]
second = [1..101]

---------------------------------------------------------------------------------------
---- Define a list of three strings: "apple", "banana", and "cherry". 
-- Write a function firstFruit that returns the first string from a list,
-- and restFruits function returns all the values except first.
-- remark: head [] and tail [] do not return normally (throw an exception)

fruits :: [String]
fruits = ["apple", "banana", "cherry"]

firstFruit :: [String] -> String
firstFruit = head

-- main = print (firstFruit ["apple", "banana", "cherry"]) -- "apple"
-- main = print (firstFruit ["orange"]) -- "orange"
-- main = print (firstFruit ["kiwi", "mango"]) -- "kiwi"

restFruits :: [String] -> [String]
restFruits = tail

-- main = print (restFruits ["apple", "banana", "cherry"]) -- ["banana", "cherry"]
-- main = print (restFruits ["orange"]) -- []
-- main = print (restFruits ["kiwi", "mango"]) -- ["mango"]

---------------------------------------------------------------------------------------
---- Write a function that puts the first value of the list as the last one, 
transformList :: [a] -> [a]
transformList [] = []                    -- Case 1: Empty list
transformList [x] = [x]                  -- Case 2: Single-element list
transformList (x:xs) = xs ++ [x]         -- Case 3: Move first element to the end

-- main = do
--     print (transformList ["a", "b", "c"])        -- ["b", "c", "a"]
--     print (transformList [True, False, True])    -- [False, True, True]
--     print (transformList [1])                    -- [1]


{-
   The following line calls transformList with an empty list.
   Since Haskell cannot infer the type of an empty list ([]), 
   we provide a type annotation (e.g., [Int]) to resolve the ambiguity.
-}

--main = print (transformList ([] :: [Int])) -- empty list
---------------------------------------------------------------------------------------

----The concatTails function takes two input lists (with length at least 2), 
--removes the first elements from each, and 
--concatenates the remaining lists. 
concatTails :: [a] -> [a] -> [a]
concatTails (_:xs) (_:ys) = xs ++ ys       -- Pattern match to get tails of both lists
concatTails _ _ = []                       -- If either list is empty, return an empty list

-- main = print (concatTails [1, 2, 3] [4, 5, 6]) -- Expected output: [2, 3, 5, 6]

-- main = print (concatTails ["a", "b", "c"] ["d", "e", "f"]) -- ["b", "c", "e", "f"]
-- main = print (concatTails [True, False] [False, True]) -- [False, True]
-- main = print (concatTails [1] []) -- "input too short"

---------------------------------------------------------------------------------------
----The concatHeads function takes the heads of both lists and returns them as a new list. 
-- Do error handling using null built-in function for empty lists.

concatHeads :: [a] -> [a] -> [a]
concatHeads xs ys
       |null xs || null ys = []
       |otherwise = [head xs, head ys]
-- main = do
--            print (concatHeads [1, 2, 3] [4, 5, 6]) -- [1, 4]
--            print (concatHeads ["a", "b", "c"] ["d", "e", "f"]) -- ["a", "d"]
--            print (concatHeads [True, False] [False, True]) -- [True, False]
--            print (concatHeads ([] :: [Int]) ([] :: [Int]) ) -- Null list
---------------------------------------------------------------------------------------
---- The averageList function takes a list of integers and returns the average of the list as a float.
-- If the list is empty, it should return 0.
-- Note: You can use fromIntegral built in function in order to convert Int to Float

averageList :: [Int] -> Float
averageList xs
       | null xs = 0.0
       | otherwise = fromIntegral (sum  xs) / fromIntegral (length xs)

-- main = do
--       print (averageList [1, 2, 3, 4, 5]) -- 3.0
--       print (averageList [10, 20, 30]) -- 20.0
--       print (averageList [7, 14, 21, 28]) -- 17.5
--       print (averageList []) -- 0.0

---------------------------------------------------------------------------------------
---- The firstLast function takes a list and returns a new list containing 
-- the first and last elements of the input list. Assume the list has at least one element.

firstLast :: [a] -> [a]
firstLast xs
       | null xs = xs
       | otherwise = [head xs, last xs]
-- main = do
--          print (firstLast "hello") -- "ho"
--          print (firstLast [1,2,3,4]) -- [1,4]
--          print (firstLast ["cad","adsa","fsafas","aa"]) -- ["cad","aa"]
--          print (firstLast [1]) -- [1,1]

---------------------------------------------------------------------------------------
---- The concatTailAndRest function takes a list and returns a new list by concatenating 
-- the tail of the list with the list excluding the last element. Assume the list is not empty.

concatTailAndRest :: [a] ->  [a]
concatTailAndRest xs
    | null xs = []
    | otherwise = tail xs ++ init xs
-- main = do
--     print (concatTailAndRest [] )
--     print (concatTailAndRest [1, 2, 3, 4, 5]) -- [2,3,4,5,1,2,3,4]
--     print (concatTailAndRest ["a", "b", "c"]) -- ["b", "c", "a", "b"]
--     print (concatTailAndRest [True, False, True]) -- [False, True, True, False]

---------------------------------------------------------------------------------------
---- The sim function takes a list of integers and returns True if the list is symmetrical, otherwise False.
-- If the list is empty, it should return True.

-- sim :: [Int] -> Bool
-- main = print (sim [1, 2, 1]) -- True
-- main = print (sim [1, 2, 3, 4, 5]) -- False
-- main = print (sim [1, 2, 2, 1]) -- True
-- main = print (sim []) -- True
-- main = print (sim [1]) -- True

---------------------------------------------------------------------------------------
---- The middle function takes a non-empty list of integers and returns the middle element.
-- If the list is empty, it should return an error message.
middle :: [Int] -> Int
middle [] = error "Your list is empty"          -- Return an error for an empty list
middle xs
    | odd len = xs !! midIndex                  -- If length is odd, return the middle element
    | otherwise = (xs !! (midIndex - 1) + xs !! midIndex) `div` 2  -- If even, return the average of the two middle elements
  where
    len = length xs                             -- Calculate the length of the list
    midIndex = len `div` 2                      -- Calculate the index of the middle element

-- main = do
--     print (middle [1..5])  -- Output: 3
--     print (middle [1..4])  -- Output: 2
--     print (middle [])      -- Error: "Your list is empty"


---------------------------------------------------------------------------------------
---- The cut function takes a list of integers and return a list of two lists, 
-- splitting the original list at the middle.
-- If the list is empty, it should return two empty lists.
cut :: [Int] -> [[Int]]
cut xs
    | null xs = [[], []]  -- Return two empty lists if the input list is empty
    | otherwise = [take midIndex xs, drop midIndex xs]  -- Split the list at the middle
  where
    midIndex = (length xs + 1) `div` 2  -- Calculate the middle index (for odd/even lengths)

-- main = do
--     print (cut [1..10])  -- Output: [[1,2,3,4,5],[6,7,8,9,10]]
--     print (cut [1..11])  -- Output: [[1,2,3,4,5],[6,7,8,9,10,11]]
--     print (cut [])       -- Output: [[], []]
--     print (cut [1])      -- Output: [[1], []]

---------------------------------------------------------------------------------------
---- The f1 function takes a list of integers and returns a new list 
-- with 3 added to every element using recursion.

fun :: [Int] -> [Int]
fun xs
   | null xs = []
   | otherwise = (head xs + 3) : fun (tail xs)

-- main = print (fun [1, 5, 3, 1, 6]) -- [4, 8, 6, 4, 9]
-- main = print (fun [0, -3, 7]) -- [3, 0, 10]
-- main = print (fun []) -- []
-- main = print (fun [10]) -- [13]

---------------------------------------------------------------------------------------
---- The f2 function will take a list of integers 
-- and returns a new list with the double of the positive elements.

f2 :: [Int] -> [Int]
f2 xs
     | null xs = []
     | head xs <= 0 = f2 (tail xs)
     | otherwise = (head xs * 2) : f2 (tail xs)

-- main = print (f2 [1, 2, -2, 3, -4]) -- [2, 4, 6]
-- main = print (f2 [0, -3, 7]) -- [14]
-- main = print (f2 []) -- []

---------------------------------------------------------------------------------------
---- The sq function will take a list of integers and returns a new list with the square of each element.

sq :: [Int] -> [Int]
sq xs
    |null xs = []
--     |otherwise = head xs * head xs : sq (tail xs)

-- main = print (sq [1..5]) -- [1, 4, 9, 16, 25]
-- main = print (sq [0, -3, 7]) -- [0, 9, 49]
-- main = print (sq []) -- []
-- main = print (sq [10]) -- [100]

---------------------------------------------------------------------------------------
---- The f4 function will take a list of lists of integers and returns a new 
-- list of lists with the square of every element.
f4 :: [[Int]] -> [[Int]]
f4 xs
      | null xs = []                                -- Return an empty list if the input list is empty
      | otherwise = map (^2) (head xs) : f4 (tail xs)-- Square the head list and recurse on the tail

-- main = do
--     print (f4 [[1, 2], [3, 4, 5, 6], [7, 8]])        -- Output: [[1, 4], [9, 16, 25, 36], [49, 64]]
--     print (f4 [[], [1], [2, 3]])                     -- Output: [[], [1], [4, 9]]
--     print (f4 [])                                     -- Output: []
--     print (f4 [[0, -1, -2]])                          -- Output: [[0, 1, 4]]


---------------------------------------------------------------------------------------
---- The not_five function will take a list of integers and returns a new list 
-- with all elements equal to 5 removed.
-- This function should not use higher-order functions.

not_five :: [Int] -> [Int]
not_five xs
    |null xs = []
    |5 == head xs  =  not_five (tail xs)
    |otherwise = head xs :not_five (tail xs)

-- not_five :: [Int] -> [Int]
-- not_five [] = []  -- Base case: if the list is empty, return an empty list
-- not_five (x:xs)
--     | x == 5    = not_five xs         -- If the head is 5, skip it and recurse on the tail
--     | otherwise = x : not_five xs     -- Otherwise, include the head and recurse on the tail

-- main = do
--     print (not_five [5, 4, 5, 4, 3])  -- Output: [4, 4, 3]
--     print (not_five [5, 5, 5])        -- Output: []
--     print (not_five [1, 2, 3, 4])     -- Output: [1, 2, 3, 4]
--     print (not_five []) -- []
--     print (not_five [5]) -- []

---------------------------------------------------------------------------------------
---- The del function will take an integer n and a list of integers, 
-- and returns a new list with all occurrences of n removed.

-- del :: Int -> [Int] -> [Int]

-- main = print (del 5 [1, 5, 6, 7, 5, 8, 5]) -- [1, 6, 7, 8]
-- main = print (del 3 [1, 2, 3, 4, 3, 5]) -- [1, 2, 4, 5]
-- main = print (del 0 [0, 0, 0, 0]) -- []
-- main = print (del 1 [2, 3, 4]) -- [2, 3, 4]
-- main = print (del 7 []) -- []


---------------------------------------------------------------------------------------
-- Insert 0 in front of every sublist of a list.
-- E.g. for [[1, 2], [3, 4], [5]] the result is [[0,1,2],[0,3,4],[0,5]]



insertZero :: [[Int]] -> [[Int]]
insertZero = map (0 :)  -- Use `map` to add `0` to the front of each sublist
-- without using higher order functions........
-- insertZero :: [[Int]] -> [[Int]]
-- insertZero [] = []  -- Base case: if the outer list is empty, return an empty list
-- insertZero (x:xs) = (0 : x) : insertZero xs  -- Prepend 0 to each inner list and recurse on the rest


-- main = do
--     print (insertZero [[1, 2], [3, 4], [5]])   -- Output: [[0,1,2],[0,3,4],[0,5]]
--     print (insertZero [[]])                    -- Output: [[0]]
--     print (insertZero [])                      -- Output: []
--     print (insertZero [[1]])                   -- Output: [[0,1]]



---------------------------------------------------------------------------------------
---- The productf function takes a list of integers and returns the product of all the elements.
-- If the list is empty, it should return 1 (the identity for multiplication).
-- ex : 3 = 1 * 2 * 3
productf :: [Int] -> Int
productf [] = 1                 -- Base case: if the list is empty, return 1
productf (x:xs) = x * productf xs  -- Recursive case: multiply the head with the product of the tail

-- main = do
--     print (productf [1, 2, 3])     -- Output: 6 (1 * 2 * 3)
--     print (productf [3, 4, 5])     -- Output: 60 (3 * 4 * 5)
--     print (productf [])            -- Output: 1
--   | null xs = 1
--   | otherwise = head xs * productf(tail xs)

-- main = print (productf[1..5]) -- 120
-- main = print (productf [1, 2, 3, 4, 5]) -- 120
-- main = print (productf [10, 20, 30]) -- 6000
-- main = print (productf []) -- 1
-- main = print (productf [7]) -- 7

---------------------------------------------------------------------------------------
---- Write a function filterEven that takes a list of integers and returns a new list containing only the even numbers.

filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven xs= [x | x <- xs, even x]
-- 2nd method without recursion....
-- filterEven xs
--       | null xs = []
--       | odd (head xs) = filterEven (tail xs)
--       | otherwise = head xs :filterEven (tail xs)


-- main = print (filterEven [1, 2, 3, 4, 5, 6]) -- [2, 4, 6]
-- main = print (filterEven [7, 9, 13]) -- []
-- main = print (filterEven [0, -2, 3, -4]) -- [0, -2, -4]
-- main = print (filterEven []) -- []

---------------------------------------------------------------------------------------
---- Write a function wordLengths that takes a list of strings and 
-- returns a list of integers representing the length of each word.

wordLengths :: [String] -> [Int]
wordLengths xs
  | null xs = []
  | otherwise = len (head xs) : wordLengths (tail xs)
-- helper fuction len to calculate size of head.............  
len :: String -> Int
len = length

-- main = print (wordLengths ["apple", "banana", "cherry"]) -- [5, 6, 6]
-- main = print (wordLengths ["hello", "world"]) -- [5, 5]
-- main = print (wordLengths ["a", "abc", ""]) -- [1, 3, 0]
-- main = print (wordLengths []) -- []

---------------------------------------------------------------------------------------
-- Write a function that removes the first and last element of a list. 
-- Assume at least 1 element is given.
removeFirstAndLast :: [a] -> [a]
removeFirstAndLast xs = tail (init xs)

-- main = do
--     print (removeFirstAndLast [1, 2, 3, 4, 5])   -- Output: [2, 3, 4]
--     print (removeFirstAndLast ["a", "b", "c"])   -- Output: ["b"]
--     print (removeFirstAndLast [True, False, True]) -- Output: [False]

---------------------------------------------------------------------------------------
-- Write a recursive function that takes a list and returns a new list with the elements in reverse order. 
-- If the list is empty, return an empty list.
-- remark: built-in function is reverse

reverseList :: [a] -> [a]
-- reverseList xs 
--  | null xs = []
--  | otherwise = reverse xs

reverseList [] = []
reverseList(x:xs) = reverseList xs ++ [x]
-- main = print (reverseList [1, 2, 3, 4]) 
-- main = print (reverseList ["apple", "banana", "cherry"]) -- ["cherry", "banana", "apple"]
-- main = print (reverseList [True, False]) -- [False, True]

---------------------------------------------------------------------------------------
-- Write a recursive function that takes a list of integers and 
-- returns a new list where each odd number is doubled, and each even number is tripled.
-- Process each integer based on whether it's odd or even
processList :: [Int] -> [Int]
processList [] = []
processList (x:xs)
    | odd x     = (2 * x) : processList xs
    | otherwise = (3 * x) : processList xs

-- -- main = do
--     print (processList [1, 2, 3, 4, 5])      -- Output: [2, 6, 6, 12, 10]
--     print (processList [10, 15, 20, 25])     -- Output: [30, 30, 60, 50]
--     print (processList [0, -3, 4, -5])       -- Output: [0, -6, 12, -10]
--     print (processList [])                   -- Output: []



---------------------------------------------------------------------------------------
-- Write a recursive function that takes a list of lists of integers and 
-- returns a new list containing the count of numbers greater than 3 in each inner list.
-- Counts numbers greater than 3 in a single list
countInList :: [Int] -> Int
countInList [] = 0
countInList (x:xs)
    | x > 3     = 1 + countInList xs
    | otherwise = countInList xs

-- Applies countInList to each list within a list of lists
countGT3 :: [[Int]] -> [Int]
countGT3 [] = []
countGT3 (xs:xss) = countInList xs : countGT3 xss

-- main = do
--     print (countGT3 [[1, 2, 3], [4, 5, 6], [2, 3, 4]]) -- Output: [0, 3, 1]
--     print (countGT3 [[0, 1, 2], [3], [4, 5]])          -- Output: [0, 0, 2]
--     print (countGT3 [[5, 6, 7], [8, 9], []])           -- Output: [3, 2, 0]
--     print (countGT3 [])                                -- Output: []



---------------------------------------------------------------------------------------
-- Write a function elementAt that takes an index n and a list and returns the element at the given index. 
-- The function should give an error message if the index is out of bounds.
-- remark: Lists are 0-indexed.
elementAt :: Int -> [a] -> a
elementAt n xs
    | n < 0 || n >= length xs = error "Index out of bounds"
    | otherwise = xs !! n

-- main = do
--     print (elementAt 2 [1, 2, 3, 4, 5])    -- Output: 3
--     print (elementAt 0 [1, 2, 3, 4, 5])    -- Output: 1
--     print (elementAt 4 "haskell")          -- Output: 'e'
    -- The following lines will produce an error due to out-of-bounds indices:
    -- print (elementAt 5 [1, 2, 3, 4, 5]) -- Error: Index out of bounds
    -- print (elementAt (-2) [1, 2, 3])    -- Error: Index out of bounds
    -- print (elementAt 5 [1, 2, 3])       -- Error: Index out of bounds


---------------------------------------------------------------------------------------

-- Write a function sliceList that takes two integers start and end, and a list. 
-- The function should return the sublist that starts at index start and ends at index end.
-- The start index must be non-negative, the end index must not be less than start, 
-- and the end index must not exceed the length of the list. If any of these conditions are violated, 
-- the function should return an error message indicating 'Invalid indices'.


sliceList :: Int -> Int -> [a] -> [a]
sliceList start end xs
    | start < 0 || end < start || end > length xs = error "Invalid indices"
    | otherwise = take (end - start) (drop start xs)

main = do
    print (sliceList 1 4 [1, 2, 3, 4, 5, 6])     -- Output: [2, 3, 4]
    print (sliceList 0 3 "haskell")              -- Output: "has"
    print (sliceList 2 4 [10, 20, 30, 40, 50])   -- Output: [30, 40]
    print (sliceList 3 1 [1, 2, 3, 4])           -- Error: Invalid indices
    print (sliceList 0 10 [1, 2, 3])             -- Error: Invalid indices

------------------------------------------------------------------------------------------

-- The firstGreaterThanFive function returns the first element greater than 5 in a list of integers. 
-- If there is no greater than 5 return -1.
firstGreaterThanFive :: [Int] -> Int
firstGreaterThanFive [] = -1
firstGreaterThanFive (x:xs)
    | x > 5     = x
    | otherwise = firstGreaterThanFive xs

-- main = do
--     print (firstGreaterThanFive [1, 2, 6, 4, 8]) -- Output: 6
--     print (firstGreaterThanFive [1, 2, 3, 4])     -- Output: -1
--     print (firstGreaterThanFive [5, 5, 7])        -- Output: 7

--------------------------------------------------------------------------------------------------------


-- The replaceFirst function replaces the first occurrence of an element in the list with a new given value.
replaceFirst :: Int -> Int -> [Int] -> [Int]
replaceFirst _ _ [] = []
replaceFirst old new (x:xs)
    | x == old  = new : xs
    | otherwise = x : replaceFirst old new xs

-- main = do
--     print (replaceFirst 3 99 [1, 2, 3, 4, 3]) -- Output: [1, 2, 99, 4, 3]
--     print (replaceFirst 5 42 [1, 2, 3, 4])    -- Output: [1, 2, 3, 4]
--     print (replaceFirst 1 0 [1, 1, 1])        -- Output: [0, 1, 1]

-----------------------------------------------------------------------------------
-- Write a function that takes a list of integers and returns a list where:
-- every even number is replaced by the character 'e' and 
-- every odd number is replaced by the character 'o'
-- 0 is replaced by 'x'. 
-- For example, the list [0,1,2,3,4,5,6] should be transformed into ['x','o','e','o','e','o','e'].
replaceInts :: [Int] -> [Char]
replaceInts [] = []
replaceInts (x:xs)
    | x == 0    = 'x' : replaceInts xs
    | even x    = 'e' : replaceInts xs
    | otherwise = 'o' : replaceInts xs

-- main = do
--     print (replaceInts [0, 1, 2, 3, 4, 5])        -- Output: "xoeoeo"
--     print (replaceInts [1, 5, 3, 2, 0, 3, 87, 1, 2, 0]) -- Output: "oooexoooex"
---------------------------------------------------------------------------------------------------

-- The doubleElements function returns a list where even elements are divided by 2 ,and odd element are multiplied by 2.
doubleElements :: [Int] -> [Int]
doubleElements [] = []
doubleElements (x:xs)
    | even x    = (x `div` 2) : doubleElements xs
    | otherwise = (x * 2) : doubleElements xs

-- main = do
--     print (doubleElements [1, 2, 3])       -- Output: [2, 1, 6]
--     print (doubleElements [-1, -2, -3])    -- Output: [-2, -1, -6]
--     print (doubleElements [0, 10, 5])      -- Output: [0, 5, 10]
--     print (doubleElements [])              -- Output: []



