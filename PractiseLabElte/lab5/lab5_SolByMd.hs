-- main :: IO ()

---- Recursion, List

-- 1. Multiply the digits of a number e.g. for 123 is 6.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
import Data.Char (intToDigit)
-- import qualified Data.ByteString as head

prodDigits :: Int -> Int
prodDigits x
  | x < 10 = x
  | otherwise = x `rem` 10 * prodDigits (div x 10)

-- main = print(prodDigits 503) -- 0
-- main = print(prodDigits 123) -- 6.
--main = print(prodDigits 54213) -- 120


-- 2. Sum numbers from 12..N in a recursive function, where N is positive.

fn :: Int -> Int
fn n
  | n <= 11 = error "N can not be zero or negative or less then 11"
  | n == 12 = 12
  |otherwise = n + fn (n-1)

--main = print(fn (-10)) -- N can not be zero or negative or less then 11
--main = print(fn 14)  -- 39


-- 3. Compute the sum 1+ 1*2 + 1*2*3+ 1*2*3*4+ 1*2*3*4*5+ ...+1*2*3*...*n 
-- where n is a positive number.
-- product function
-- sump :: Int -> Int
-- sump n =sum [product [1..k] | k<-[1..n]]
-- with recursion

-- sump 1= 1
-- sump x = factorial x + sump(x-1)

-- factorial:: Int -> Int
-- factorial 1 = 1
-- factorial n = n*factorial(n-1) 
-- main = print(sump 5) -- 153

----Higher order function
-- 4. Cut a list in 4 parts quarter, middle, third quarter. 
-- E.g. cut [1..10] -> [[1,2], [3,4,5], [6,7], [8,9,10]]
cut :: [Int] -> [[Int]]
cut xs =
  let len = length xs
      q = len `div` 4
      r = len `mod` 4
      firstQuarter = take q xs
      middle = take (q + (if r > 0 then 1 else 0)) (drop q xs)
      thirdQuarter = take q (drop (q + length middle) xs)
      lastQuarter = drop (q + length middle + q) xs
  in [firstQuarter, middle, thirdQuarter, lastQuarter]

-- main :: IO ()
-- main = do
--   print (cut [1..10])    -- [[1,2],[3,4,5],[6,7],[8,9,10]]
--   print (cut [1..11])    -- [[1,2],[3,4,5],[6,7,8],[9,10,11]]
--   print (cut [])         -- [[],[],[],[]]
--   print (cut [21])       -- [[],[],[],[21]]
--   print (cut [1..21])    -- [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20,21]]

-- 5. Extract the third element of a non-empty list. 
-- use !!
m2 :: [Int] -> Int
m2 (_:_:x:_) = x  -- Pattern matching to extract the third element
m2 _ = error "Your list must have at least 3 elements!"  -- Error for invalid cases

-- main = do
--   print (m2 [1..5]) -- Output: 3
--   print (m2 [1..4]) -- Output: 3
--   print (m2 [1])    -- Error: "Your list must have at least 3 elements!"
--   print (m2 [])     -- Error: "Your list must have at least 3 elements!"

-- solutions using HOF
-- m2HOF :: [Int] -> Int
--   | length ls < 3 = error "Your list must have at least 3 elements!"
--   | otherwise = ls !! 2  -- Use `!!` to access the third element (0-based indexing)
---- Higher order functions

-- Earlier exemples rewritten with higher order functions: map, foldr, filter, takeWhile. 
-- Operations with lists: write functions for the followings


-- 1.Keep the head of every sublist (assume sublists are not empty).
-- e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
heads :: [[Int]] -> [Int]
heads xs = map head xs
-- main = print(heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]) -- [1,3,5]
-- 2. Keep the tails of a list.
-- e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [[2, 3], [4], [7, 8, 9]] 
tails :: [[Int]] -> [[Int]]
-- tails [] = []
-- tails (x:xs) = tail x : tails xs  
-- using high order functions
tails = map tail

-- main = print(tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]]) -- [[2, 3], [4], [7, 8, 9]] 


-- 3. Add 100 to the numbers of a list.
-- g :: Int -> Int

-- add100 :: [Int] -> [Int]
-- add100 [] = []
-- add100(x:xs) = x+100 : add100 xs
-- with high order functions
-- add100[] = map (+100)
--add100' :: [Int] -> [Int]


-- main = print(add100 [1..8]) -- [101,102,103,104,105,106,107,108]
--main = print(add100' [1..8]) -- [101,102,103,104,105,106,107,108]

-- 4. Triple the elements of a list.
triples :: [Int] -> [Int]
triples = map (*3)
-- main = print(triples [1..5]) -- [3,6,9,12,15]


-- 5. Check if the numbers of a list are odd.
isoddnrs :: [Int] -> [Bool]
-- isoddnrs [] = []
-- isoddnrs(x:xs) = if odd x then True : isoddnrs xs else False: isoddnrs xs
isoddnrs = map odd
--isoddnrs2 :: [Int] -> [Bool]

-- main = print(isoddnrs [1..5]) -- [True,False,True,False,True]
--main = print(isoddnrs2 [1..5]) -- [True,False,True,False,True]


-- two ways of writing the function
-- 6. Check if the numbers of a list are multiple of 10.

ismult10 :: [Int] -> [Bool]
ismult10 = map (\x -> rem x 10 == 0)
-- main = print (ismult10 [1..20])
--[False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,True]


-- 7. Collect in a list the last digits of the numbers of a list.

lastdigits :: [Int] -> [Int]
lastdigits [] = []
lastdigits (x:xs)
  |x < 10 = x : lastdigits xs
  |otherwise = x `mod` 10: lastdigits xs
  
-- main = print (lastdigits [1..35])
-- [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]


-- 8. Compute the cube of the numbers of a list.
cubes :: [Int] -> [Int]
cubes = map(^3)
-- main = print(cubes [1..10]) -- [1,8,27,64,125,216,343,512,729,1000]
--main = print(cubes []) -- []

-- 9. Reverse every sublist of a list.
revsub :: [[Int]] ->  [[Int]]
revsub xs = map reverse xs
-- main = print(revsub [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[3,2,1],[6,5],[],[10,9,8,7]]

-- 10. Keep the last elements of the sublists of a list in one list 
-- (assume the sublists are not empty).
-- E.g. [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
--lasts :: [[Int]] -> [Int]


--main = print(lasts [[1,2,3],[5,6],[1],[7,8,9,10]]) -- [3,6,1,10]



-- 11. Delete the last element of each sublist of a list.
-- E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] -> [[1,2],[5],[],[7,8,9]]
--lastdel :: [[Int]] -> [[Int]]


--main = print(lastdel [[1,2,3],[5,6],[7,8,9,10]]) -- [[1,2],[5],[7,8,9]]


-- 12. Instert 0 in front of every sublist of a list.
-- E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is 
-- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
--ins0 :: [[Int]] -> [[Int]]


--main = print(ins0 [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]


-- insert 0 at the end!!
--ins0' :: [[Int]] -> [[Int]]


--main = print(ins0' [[1,2,3],[5,6],[],[7,8,9,10]]) -- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]


-- 13. Compute the squares of the elements of a list using map.
-- [1, 2, 3] -> [1, 4, 9]
--sq :: Int -> Int


--sqrs :: [Int] -> [Int]


--main = print(sqrs [1..10]) -- [1,4,9,16,25,36,49,64,81,100]


-- 14. Same as 13. with lambda expression.
--sqrs_lambda :: [Int] -> [Int]


--main = print(sqrs_lambda [1..10]) -- [1,4,9,16,25,36,49,64,81,100]


-- review foldr
--main = print(foldr (+) 1 [4,5,6])  --(4 + (5 + (6 + 1))) 16


-- 15.  Add the numbers from 1..N (N positive and not 0) using foldr.
--addn :: Int -> Int


--main = print(addn 5) -- 15
--main = print(addn 0) -- 0
--main = print(addn (-2))  -- 0
--main = print(addn 10) --55


-- 16. Compute the product of the elements of a list using foldr.
prodf :: [Int] -> Int
prodf = foldr (*) 1

prodf2 :: [Int] -> Int
prodf2 = product


--main = print(prodf [1,5,2,4]) -- 40
--main = print(prodf2 [1,5,2,4]) -- 40


-- 17. Compute 1*1 + 2*2 + ... + n*n  for n positive using map and foldr.
--sumsqr :: Int -> Int


-- creates the [1..5] list
-- then maps x*x to every element from the list [1,4,9,16,25]
-- then sums up using foldr 55

--main = print(sumsqr 5) -- 55
--main = print(sumsqr 0) -- 0 -- the list is [1..0] which is empty, then foldr has as result 0
--main = print(sumsqr (-4)) -- 0 -- same here

-- *
-- 18. Write a function for the square of every element of a list and sublists.
-- [[1,2],[3,4,5,6],[7,8]]  ->  [[1,4],[9,16,25,36],[49,64]]  
-- hint: map in map
--fa :: [Int]-> [Int]


--main = print(fa [1..5]) --[1,4,9,16,25]

--f4 :: [[Int]] -> [[Int]]


--f4' :: [[Int]] -> [[Int]]


--main = print(f4 [[1,2],[3,4,5,6],[7,8]]) -- [[1,4],[9,16,25,36],[49,64]]
--main = print(f4' [[1,2],[3,4,5,6],[7,8]]) -- [[1,4],[9,16,25,36],[49,64]]


-- 19. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

rep :: Int -> Int -> [Int]
rep 0 x = []
rep n x = x : rep (n-1) x

--main = print(rep 3 7) -- [7,7,7]

rep1 :: Int -> Int -> [Int]
rep1 = replicate   -- built-in replicate

--main = print(rep1 3 7) -- [7,7,7] 

f5 :: Int -> [Int] -> [[Int]]
--f5 n x = map (\ x = rep n x) x
f5 n = map (rep n)   -- partial parameterization !!!

--main = print(f5 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

f51 :: Int -> [Int] -> [[Int]]
f51 n = map (rep1 n)

--main = print(f5 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

f52 :: Int -> [Int] -> [[Int]]
f52 n = map (replicate n)

--main = print(f51 3 [3..6]) -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

-- review filter
-- 20. Compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
-- hints: first filter it then use map 


--main = print(f20 [1, 2, (-2), 3, (-4)]) -- [2, 4, 6]

--f20' :: [Int] -> [Int]


--main = print(f20' [1, 2, (-2), 3, (-4)]) -- [2, 4, 6]


-- 21. Filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]
--f7 :: Int -> [Int] -> [Int]

--main = print(f7 3 [1,5,3,2,1,6,4,3,2,1])  -- [1,2,1,2,1]


-- 22. Using notempty, eliminate the empty lists from a list of lists. 
-- [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

notempty :: [Int] -> Bool
notempty x = x /= []

--f8 :: [[Int]] -> [[Int]]


--main = print(f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]) -- [[1,2,3],[3,4,5],[2,2]]


-- 23. Compute the sum of the sublists using foldr [[1,2,3], [3,4,5], [2,2]] -> [6, 12, 4]


-- main = print(f9 [[1,2,3], [3,4,5], [2,2]]) -- [6,12,4]


-- 24. Write a function that keeps the integers of a list up to the first 0 encounterred 
-- and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
-- hints: use takeWhile then map
--f3 :: [Int] -> [Int]


--main = print(f3 [1, 2, (-2), 3, 0, (-4)]) -- [0, 1, -1, 1]


-- 25. Insert the sum of elements of the sublist as last element in every sublist of a list.
--insLast :: [Int] -> [Int] 


--insSum :: [[Int]] -> [[Int]]


--insSum2 :: [[Int]] -> [[Int]]


--main = print(insSum [[1,2], [3,4,5], [6,5,9,7], [], [8]]) -- [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]
--main = print(insSum2 [[1,2], [3,4,5], [6,5,9,7], [], [8]]) -- [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]

-- ***
-- 26. Write a function that checks if each elements in the list appear even times.
-- E.g. checkEven [1,1,2,2,2,2,3,5,3,5] = True  
--checkAux :: [Int] -> Int -> Bool


--checkEven :: [Int] -> Bool


--main = print(checkEven [1,1,2,2,2,2,3,5,3,5]) -- True
--main = print(checkEven [1,1,2,2,1]) -- False
--main = print(checkEven []) -- True


-- 27. Insert x as second element in every sublist of a list.
-- if the sublist was empty then x will be the only element in the new sublist. 
-- [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]
--insAux :: Int -> [Int]  -> [Int]


--insertAtTwo :: [[Int]] -> Int -> [[Int]]


--main = print(insertAtTwo [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10)
-- [[1,10,2],[3,10,4,5],[6,10,5,9,7],[10],[8,10]]


-- 28. Given a list of lists, for each list, extract the first, middle and last element.
--extract3 :: [[Int]] -> [[Int]]


--main = print(extract3 [[1..9], [2..6], [3..11], [1..10]]) -- [[1,5,9],[2,4,6],[3,7,11],[1,6,10]]
--main = print(extract3 []) --[]
