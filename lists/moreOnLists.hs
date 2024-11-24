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