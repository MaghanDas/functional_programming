main :: IO ()

{-  Lists - review basics
-- empty list is []
-- every list has a type, the type of the contained elements
-- singleton list with one element [False], [[1,2,3]]
-- special constructor is 1:[2,3,4] is equivalent to [1,2,3,4]
-- [1,2,3] is equivalent to 1:2:3:[]
-- 1 : 2 : 3 : [] == 1 : (2 : (3 : [])) == [1, 2, 3]
-}

-- add a single element to the beginning of a list
l1 = 1:[3,4,5]
--main = print l1 -- [1,3,4,5]

l2 = 1 : 2 : 3 : []
--main = print l2 -- [1,2,3]

l3 =  1 : (2 : (3 : []))
--main = print l3 -- [1,2,3]

b :: Bool
b = (1 : 2 : 3 : []) == (1 : (2 : (3 : [])))
b2 = ((1 : (2 : (3 : []))) == [1, 2, 3])
b3 = ( (1 : 2 : 3 : []) == (1 : (2 : (3 : []))))  && ((1 : (2 : (3 : []))) == [1, 2, 3])
--main = print (b && b2) -- True
--main = print (b3) -- True

-- Dot-dot generator: finite and infinite list generator

--main = print [0,2..20]      -- [0,2,4,6,8,10,12,14,16,18,20]
--main = print [1000..]     -- [1000,1001,1002,..] and so on, infinite
--main = print [1000, 900..] 
--The elements need not be constants, used expressions must be of the same type

lfloats :: [Float]
lfloats = [sqrt 2, 5/2, max 5.0 55.0 ]
--main = print lfloats   -- [1.4142135,2.5,55.0]

------ Basic built-in functions - review

--main = print (head [1, 2, 3, 4, 5] )    -- 1 first element of the list
--main = print (tail [1, 2, 3, 4, 5] )  -- [2,3,4,5] everything except first element as a list
--main = print (drop 2 [1, 2, 3, 4, 5] ) -- [3,4,5] delete first n elements
--main = print (take 2 [1, 2, 3, 4, 5] ) -- [1,2] take first n elements
--main = print ([1, 2, 3] ++ [4, 5, 6] ++ [10]) -- [1,2,3,4,5,6,10] concatenation, appends the lists
--main = print (reverse [1, 2, 3])  -- [3,2,1] reverses a list
--main = print (length [1, 2, 3, 4] )   -- 4 number of elements
--main = print (last [1, 2, 3])   -- 3 last list element     
--main = print (init [1, 2, 3])  -- [1,2] all except the last          
--main = print ( elem 2 [1, 2, 3] ) -- True, check membership
--main = print ( elem 5 [1, 2, 3]) -- False
--main = print (concat [[1,2], [3, 4, 5], [6, 7]] ) -- [1,2,3,4,5,6,7] flattens a list of lists


-- More tests

--main = print (take 2 ([]::[Int]))   -- [] empty, it can not take any from []   -- not working without extra specification for empty list    
--main = print (drop 5 [1,2,3])  -- [] empty, it can not drop 5 out of 3       
--main = print (take 2 [1 .. 10])  -- [1,2]       
--main = print (drop ([1..5]!!2) [1..5]) -- [4,5] the !! is list index (subscript) operator, starting from 0, shows n+1-th element

--main = print (reverse [5,4 .. -5])  -- [-5,-4,-3,-2,-1,0,1,2,3,4,5]      
--main = print (elem 0 [])  -- False             
--main = print (elem (-1) [1..10])     -- False     
--main = print (elem ([1..5]!!1) [1..5])  -- True, extracts an element and checks if is member -> true


-- rewriting built-in functions, names should not overlap
head1 :: [Int] -> Int
head1 (x : y) = x

--main = print (head1 [1..5]) -- 1

tail1 :: [Int] -> [Int]
tail1 (x : y) = y

--main = print (tail1 [1..5]) -- [2,3,4,5]

take1 :: Int -> [a] -> [a]
take1 n [] = []
take1 n (x : xs)
 | n < 1 = []
 | otherwise = x : take1 (n-1) xs

-- main = print (take1 2 [1..5]) -- [1,2]
--main = print (take1 2 [1 .. 10]) -- [1,2]
--main = print (take1 7 [1..5]) -- [1,2,3,4,5]

drop1 :: Int -> [a] -> [a]
drop1 n [] = []
drop1 n ( x : xs)
 | n < 1 = x : xs
 | otherwise = drop1 (n-1) xs

-- main = print (drop1 2 [1,2,3]) -- []
--main = print (drop1 ([1..5]!!2) [1..5]) -- [4,5]
--main = print (drop1 0 [1..5])  -- [1,2,3,4,5]

{- drop1 2 [1..5]
n = 2 x = 1 xs = [2,3,4,5]     drop1 1 [2 ,3 ,4 ,5]
n = 1 x = 2 xs = [3 ,4 ,5]     drop1 0 [3 ,4 ,5]
n = 0 < 1                      [3 ,4 ,5]
-}

reverse1 :: [a] -> [a]
reverse1 [] = []
-- reverse1(x:xs)= reverse xs \\ using built in reverse functions
-- reverse1 (x : xs) = reverse1 xs ++ [ x ] \\ without built in reverse

-- main = print (reverse1 [1..10]) -- [10,9,8,7,6,5,4,3,2,1]
--main = print (reverse1 []:: [Int]) -- []
--main = print (reverse1 [5 ,4 .. -5]) -- [-5,-4,-3,-2,-1,0,1,2,3,4,5]

-- some more list patterns
-- the input is expected in a set format = pattern 
-- triplesum :: [Int] -> Int
-- triplesum = sum -- or
-- triplesum [x, y, z] = x + y + z

-- main = print (triplesum [1,2,4])  -- 7  
-- main = print (triplesum [1,2,3,4])  -- error, Non-exhaustive patterns in function triplesum

-- omitting values
f :: Int -> Int -> Int
f  _  x = x
-- main = print (f 4 5) -- 5
