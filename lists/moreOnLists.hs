-- In Haskell, you can work with a list of sublists (a nested list) by applying functions to either the sublists or 
-- their elements. This is achieved using higher-order functions like map, filter, and recursion.

-- Examples of Operations on Sublists and Their Elements

-- 1. Filtering Even Numbers from Sublists
-- filterEvensFromSublists :: [[Int]] -> [[Int]]
-- filterEvensFromSublists = map (filter even)

-- main = print $ filterEvensFromSublists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- Explanation:

-- filter even takes a sublist and removes all odd numbers.
-- map applies filter even to each sublist.
-- Input: [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- Output: [[2], [4, 6], [8]]

--  reversing each sublist
-- reverseSublists :: [[a]] -> [[a]]
-- reverseSublists = map reverse
-- main = print $ reverseSublists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- reverse reverses a single sublist.
-- map reverse applies reverse to each sublist

-- 3. Combining Operations: Filter Even Numbers and Reverse Sublists
-- processSublists :: [[Int]] -> [[Int]]
-- processSublists = map (reverse . filter even)

-- main = print $ processSublists [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- Explanation:

-- filter even keeps only even numbers from a sublist.
-- reverse then reverses the filtered sublist.
-- map (reverse . filter even) applies this combined operation to each sublist.
-- Input: [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- Output: [[2], [6, 4], [8]]

-- Flattening a Nested List and Filtering Even Numbers
-- filterEvensFromNestedList :: [[Int]] -> [Int]
-- filterEvensFromNestedList = filter even . concat

-- main = print $ filterEvensFromNestedList [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- concat flattens [[1, 2, 3], [4, 5, 6], [7, 8, 9]] into [1, 2, 3, 4, 5, 6, 7, 8, 9].
-- filter even then extracts even numbers

-- 5. Sorting Each Sublist
-- import Data.List (sort)

-- sortSublists :: [[Int]] -> [[Int]]
-- sortSublists = map sort

-- main = print $ sortSublists [[3, 1, 2], [6, 4, 5], [9, 8, 7]]

-- The key benefit of $ is that it allows you to avoid parentheses when applying functions, as it has the lowest precedence.
-- print (sort (concat [[3, 1], [2, 4]]))
-- print $ sort $ concat [[3, 1], [2, 4]]
-- $ replaces parentheses, making the code more readable.
-- The expression is evaluated from right to left.

-- Example: Doubling every element in a list of lists of lists
doubleNestedList :: [[[Int]]] -> [[[Int]]]
doubleNestedList = map (map (map (*2)))

-- main :: IO ()
-- main = print $ doubleNestedList [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]

-- Explanation:
-- map (*2) is applied to the innermost list (elements of type Int), doubling each value.
-- The second map applies the function to each inner list (i.e., a list of integers).
-- The outer map applies the operation to each outer list (i.e., the list of lists of integers).
-- Input: [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
-- Output: [[[2, 4], [6, 8]], [[10, 12], [14, 16]]]

-- Example: Uppercasing and reversing strings in a list of lists of strings
-- processStrings :: [[String]] -> [[String]]
-- processStrings = map (map (reverse . map toUpper))
-- main :: IO ()
-- main = print $ processStrings [["hello", "world"], ["haskell", "rocks"]]
-- Explanation:
-- map toUpper turns each character in a string to uppercase.
-- reverse reverses each string.
-- The second map applies this to each string in a sublist of strings.
-- The outer map applies this operation to each list of strings.
-- Input: [["hello", "world"], ["haskell", "rocks"]]

-- Output: [["OLLEH", "DLROW"], ["LEKSAAH", "SKCOR"]]

-- Example: Filtering odd numbers and multiplying by 10
-- processNumbers :: [[Int]] -> [[Int]]
-- processNumbers = map (map (\x -> if even x then x * 10 else x))

-- main :: IO ()
-- main = print $ processNumbers [[1, 2, 3], [4, 5, 6], [7, 8, 9]] --[[1, 20, 3], [40, 5, 60], [7, 80, 9]]

-- Example: Adding 5 to each number in the list inside each tuple
-- processTuples :: [(String, [Int])] -> [(String, [Int])]
-- processTuples = map (\(s, lst) -> (s, map (+5) lst))

-- main :: IO ()
-- main = print $ processTuples [("A", [1, 2, 3]), ("B", [4, 5, 6])] --[("A", [6, 7, 8]), ("B", [9, 10, 11])]

-- Example: Filtering negative numbers, squaring, and reversing
-- processNumbers2 :: [[Int]] -> [[Int]]
-- processNumbers2 = map (reverse . map (^2) . filter (>= 0))

-- main :: IO ()
-- main = print $ processNumbers2 [[-1, 2, 3], [4, -5, 6], [7, 8, -9]]
-- Explanation:
-- filter (>= 0) filters out negative numbers.
-- map (^2) squares each remaining number.
-- reverse reverses the order of numbers in each sublist.
-- These functions are composed together using . (function composition), and map applies them to each sublist.
-- Input: [[-1, 2, 3], [4, -5, 6], [7, 8, -9]]

-- Output: [[9, 4], [36, 16], [49, 64]]


-- 12. Instert 0 in front of every sublist of a list.
-- -- E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is 
-- -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
-- ins0 :: [[Int]] -> [[Int]]
-- -- ins0  = map (0: )
-- -- ins0  = map (++ [0])

-- main = print(ins0 [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]

-- concatMap is a higher-order function in Haskell that combines the behavior of map and concat. It applies a function to each element of a list, 
-- producing a list of lists, and then concatenates these lists into a single flattened list.

-- The type signature is:
-- concatMap :: (a -> [b]) -> [a] -> [b]  
-- (a -> [b]): A function that transforms each element of the input list into a list.
-- [a]: The input list.
-- [b]: The resulting flattened list after mapping and concatenating.
-- concatMap f xs = concat (map f xs)
-- How It Works
-- map f xs: Applies f to each element of the list, resulting in a list of lists.
-- concat: Flattens this list of lists into a single list.

-- duplicate :: [Int] -> [Int]
-- duplicate = concatMap (\x -> [x, x])
-- -- Usage:
-- duplicate [1, 2, 3] -- Output: [1, 1, 2, 2, 3, 3]

-- pairs :: [a] -> [b] -> [(a, b)]
-- pairs xs ys = concatMap (\x -> map (\y -> (x, y)) ys) xs

-- -- Usage:
-- pairs [1, 2] ['a', 'b'] -- Output: [(1,'a'), (1,'b'), (2,'a'), (2,'b')]

-- import Data.List (group)

-- checkEven :: [Int] -> Bool
-- checkEven list = all (even . length) (group (sort list))

-- main :: IO ()
-- main = print (checkEven [1, 1, 2, 2, 2, 2, 3, 5, 3, 5]) -- True

-- group (sort list):
-- The sort function sorts the list so that identical elements are adjacent to each other.
-- group then groups identical elements together into sublists. For example, [1, 1, 2, 2, 3] becomes [[1, 1], [2, 2], [3]].
-- all (even . length):
-- For each group (which is a sublist of identical elements), length calculates how many times that element appears.
-- even checks if this length is even.
-- all ensures that all groups have even lengths.

-- import Data.List (group, sort)


-- -- Function to count occurrences of each element
-- countOccurrences :: [Int] -> [(Int, Int)]
-- countOccurrences list = map (\xs -> (head xs, length xs)) (group (sort list))

-- -- Usage:
-- main :: IO ()
-- main = print (countOccurrences [1, 1, 2, 2, 2, 2, 3, 5, 3, 5])

-- Explanation
-- Sorting:
-- sort list sorts the list so that identical elements are next to each other. For example, [1, 1, 2, 2, 3, 5, 3, 5] becomes [1, 1, 2, 2, 3, 3, 5, 5].
-- Grouping:
-- group (sort list) groups the identical elements together. After grouping, the list [1, 1, 2, 2, 3, 5, 3, 5] becomes [[1, 1], [2, 2], [3, 3], [5, 5]].
-- Counting Occurrences:
-- map (\xs -> (head xs, length xs)) applies a function to each group of identical elements.
-- For each group (xs), head xs gives the element itself (the value of the group), and length xs gives the count of how many times that element appears.
-- This gives a list of tuples, where each tuple is an element and its count. For example, [(1, 2), (2, 4), (3, 2), (5, 2)].

