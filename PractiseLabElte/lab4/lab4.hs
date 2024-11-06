-- If you have no idea, write down your ideas/process by hand first!
-- Simulate process to solve the task!

-- 1 ----------------------------------------------------------------------
-- Write a recursive function that computes the n-th multiple of an x plus 10 (n*x+10).

f1 :: Int -> Int -> Int
f1 _ 0 = 10
f1 a b = a + f1 a (b-1)
-- 5 * 2 + 10 
-- 5 + 5 
-- f1 :: Int -> Int -> Int
-- f1 0 _ = 10
-- -- f1 1 b = b + 10
-- f1 a b = b + f1 (a - 1) b

-- main = print (f1 5 2) -- 20
-- 5 * 2 + 10 (a * b + 10)
--- 2  + 2  + 2 + 2 +2

---------------------------------------------------------------------------------------
---- The concatTailAndRest function takes a list and returns a new list by concatenating 
-- the tail of the list with the list excluding the last element. if the list is empty. output error

concatTailAndRest :: [a] -> [a]
concatTailAndRest (x : xs) = xs ++ init (x : xs) 
-- [1 2 3 4 5]
-- [2 3 4 5] ++ [1 2 3 4]
--concatTailAndRest  l = tail l ++  init l
-- x here is the same as (head l)
-- xs is the same as (tail l)
    --tail (x : xs) ++ init (x : xs)

-- main = print (concatTailAndRest []) -- doesn't work for [a] -> [a] ,you can try [Int] -> [Int]
-- main = print (concatTailAndRest [1, 2, 3, 4, 5]) -- [2,3,4,5,1,2,3,4]
-- main = print (concatTailAndRest ["a", "b", "c"]) -- ["b", "c", "a", "b"] -- "bcab"
-- main = print (concatTailAndRest [True, False, True]) -- [False, True, True, False]

-- 2 ----------------------------------------------------------------------
-- Add 2 to every odd number of a list, and subtract 2 from every even number.

f2 :: [Int] -> [Int]
f2 [] = []
f2 (x : xs) 
    | odd x = (x+2) : f2 xs
    | otherwise = (x - 2) : f2 xs

-- {-
-- f2 [] = []
-- f2 (x:xs) = 4*fromEnum
-- -}
-- main = print (f2 [1..5]) -- [3,0,5,2,7]



-- 3 ----------------------------------------------------------------------
-- Compute the triple of the negative elements of a list up to the first positive number.

f3 :: [Int] -> [Int]
f3 [] = []
f3 (x : xs) 
    | x < 0 = (3 * x) : f3 xs
    | otherwise = []

-- main = print (f3 [-1,-3,-5,-5,2,-4,-5]) -- [-3, -9, -15, -15]

-- 4 ----------------------------------------------------------------------
-- Write a function that keeps the non-zero elements of a list and then multiply by 2 every element.

f4 :: [Int] -> [Int]
f4 [] = []
f4 (x : xs) 
    | x /= 0 = (x * 2) : f4 xs 
    | otherwise = f4 xs

-- main = print (f4 [1,2,3,0,5,0,6,0,0,0,0]) -- [2,4,6,10,12]


-- 5 ----------------------------------------------------------------------
-- Write a function for the square, the cube, and so on up to the n-th power of a number,
-- so that increasing powers of a number are obtained in a list.
f5 :: Int -> Int -> [Int]
f5 1 x = []
f5 n x = f5 (n - 1) x ++ [x ^ n]

-- main = print (f5 5 2)  -- [4,8,16,32]

-- 6 ----------------------------------------------------------------------
-- Replicate n>0 times a list.
f6 :: Int -> [Int] -> [[Int]]
f6 0 x = []
f6 n x = x : f6 (n - 1) x

-- main = print (f6 3 [1..5]) -- [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]

-- 8 ----------------------------------------------------------------------
-- Extract the elements smaller then the head element of a list. Assume that the list is not empty.
f8 :: [Int] -> [Int]
f8 [] = []
f8 (h : t) = f8a h t

f8a :: Int -> [Int] -> [Int]
f8a n [] = []
f8a n (x : xs)
    | x < n = x : f8a n xs
    | otherwise = f8a n xs

-- main = print (f8 [5,1,2,3,4,5,3,6,7,1,8]) -- [1,2,3,4,3,1]

-- 21 ----------------------------------------------------------------------
-- Delete every second element from a list.
-- (x1 : x2 : xs)

del2 :: [Int] -> [Int]
del2  [] = []
del2  [x] = [x]
del2  (x : y : xs) = x : del2 xs
-- main = print (del2 [1..10]) -- [1,3,5,7,9]
-- main = print (del2 [1..11]) -- [1,3,5,7,9,11]
-- length  (x : y : xs) == 11 
-- 1 : length xs == 9
-- ... length xssss == 1

-- 10 -> 8 -> 0


-- 11 ----------------------------------------------------------------------
-- Given a list of Ints, remove the element at the given position.
remElemAt :: Int -> [Int] -> [Int]
remElemAt i list = take i list ++ drop (i + 1) list

-- main = print (remElemAt 6 [1..7]) -- [1,2,3,4,5,6]
-- main = print (remElemAt 2 [1..7]) -- [1,2,4,5,6,7]
-- main = print (remElemAt 9 [1..7]) -- [1,2,3,4,5,6,7]

-- 22 ----------------------------------------------------------------------
-- Delete every third element of the sublists of a list.
delete_3 :: [Int] -> [Int]
delete_3 [] = []
delete_3 [x] = [x]
delete_3 [x, y] = [x, y]
delete_3 (x : y : z : t) = x : y : delete_3 t

-- main = print (delete_3 [1..15]) -- [1,2,4,5,7,8,10,11,13,14]

delete_3' :: [Int] -> [Int]
delete_3' [] = []
delete_3' x = remElemAt 2 x

del3 :: [[Int]] -> [[Int]]
del3 [] = []
del3 (x : xs) = delete_3 x : del3 xs -- delete_3' x : del3 xs

-- main = print (del3 [[1..5],[],[1..4],[1,5],[1],[1..3],[1..10]])
-- [[1,2,4,5],[],[1,2,4],[1,5],[1],[1,2],[1,2,4,5,7,8,10]]


-- 12 ----------------------------------------------------------------------
-- Switch places the first and last element of a 3 element list.

reorder :: [String] -> [String]
reorder [t, b, h] = [h, b, t] -- reorder (t:b:h:[]) = h:b:t:[]

-- main = print (reorder ["tail", "body", "head"])              -- ["head", "body", "tail"]
-- main = print (reorder ["tails", "body", "heads"] )           -- ["heads", "body", "tails"]
-- main = print (reorder ["ground", "rainbow", "sky"])          -- ["sky", "rainbow", "ground"]



-- 14 ----------------------------------------------------------------------
-- Given a list of integers, find the minimum of a list (assume the list is not empty).
minimum1 :: [Int] -> Int
minimum1 [x] = x
minimum1 (x : y : xs)
    | x < y = minimum1 (x : xs)
    | otherwise = minimum1 (y : xs)

-- [1,0,3,4,5]
-- minimum1 [0,3,4,5]
-- minimum1 [0,4,5]
-- minimum1 [0,5]
-- minimum1 [0]

-- main = print (minimum1 [1..5])        -- 1
-- main = print (minimum1 [10,9,8,7,6])  -- 6
-- main = print (minimum1 [8,6,4,10,12]) -- 4

minimum2 :: [Int] -> Int
minimum2 [x] = x
minimum2 (x : xs) = min x (minimum2 xs)

-- minimum2 [1,2,0,-1] -> min 1 (min 2 (min 0 (-1)))
-- minimum2 [1,2,0,-1] -> min 1 (min 2 -1)
-- minimum2 [1,2,0,-1] -> min 1 -1
-- minimum2 [1,2,0,-1] -> -1

-- main = print (minimum2 [1..5])        -- 1
-- main = print (minimum2 [10,9,8,7,6])  -- 6
-- main = print (minimum2 [8,6,4,10,12]) -- 4

minimum3 :: [Int] -> Int
minimum3 x = minimum x

-- main = print (minimum3 [1..5]) -- 1
