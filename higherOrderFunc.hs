

-- Higher order functions

-- map - applies a function f to each element of a list
-- map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- map f [x1, x2, ...] == [f x1, f x2, ...]
-- map :: (a->b) -> [a] -> [b]

map1:: (a->b) -> [a] -> [b]
map1 = map

fm :: Int -> Int
fm x = 2*x
-- main = print (map1 fm [1..10]) -- [2,4,6,8,10,12,14,16,18,20]
-- main = print (map fm [1..10]) --[2,4,6,8,10,12,14,16,18,20]

fm2 :: Int -> Float
fm2 x = fromIntegral x / 2
--main = print (map fm2 [1..10]) -- [0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]


-- Lambda expressions

-- An anonymous function is a function without a name. 
-- It is a lambda abstraction and might look like this: \x -> x + 1
-- \ is symbolic way writing the lambda letter

-- this is a nameless function which increments its parameter x
la = (\x -> x + 1) 4
--main = print (la) -- 5

-- this a nameless function of two parameters, x and y
la2 = (\x y -> x + y) 3 5
--main = print (la2) -- 8

-- this a nameless function to add 10 to the argument x
la3 = map (\x -> 10 + x) [1..5]
--main = print (la3) -- [11,12,13,14,15]

-- the above can be rewritten in "pointfree style" or with partial parameters
-- (\x -> 10 + x) is equivalent to  (+10) 
la4 = map (+10) [1..5]
--main = print (la4) -- [11,12,13,14,15]


lm1 = map (+1) [1, 2, 3] -- [2,3,4]
lm2 = map id [1, 2, 3] -- [1,2,3] -- id x = x
lm3 = map (\n -> 3 * n + 1) [1, 2, 3] --[4,7,10]
--main = print ([lm1, lm2, lm3]) -- [[2,3,4],[1,2,3],[4,7,10]]


-- filter - applied to a predicate and a list, returns the list of those elements that satisfy the predicate
-- filter :: (a -> Bool) -> [a] -> [a]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x : xs)
  | p x = x : filter1 p xs
  | otherwise = filter1 p xs

ff :: Int -> Bool
ff x = x `rem` 2 == 0
--main = print (ff 23) -- False
--main = print (filter1 ff [1..10]) -- [2,4,6,8,10]
--main = print (filter1 ff [3,5,7,2,9]) -- [2]


fil1 = filter even [1, 2, 3] -- [2]
--main = print (fil1)
fil2 = filter (\l -> length l > 3) ["Hello", ", ", "World", "!"] -- ["Hello","World"]
--main = print (fil2)
fil3 = filter (/= 3) [1, 2, 3, 4, 3, 2, 1] -- [1,2,4,2,1]
--main = print (fil3)


-- takeWhile -  applied to a predicate p and a list xs, returns the longest prefix (possibly empty) 
-- of xs of elements that satisfy p
-- takes the elements of the list that satisfy teh property
-- takeWhile :: (a -> Bool) -> [a] -> [a]

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x : xs)
  | p x = x : takeWhile1 p xs
  | otherwise = []

--main = print (takeWhile1 ff [1..10]) -- []
--main = print (takeWhile1 odd [1..10]) -- [1]

t1 = takeWhile (< 3) [1,2,3,4,1,2,3,4] -- [1,2]
t2 = takeWhile (< 9) [1,2,3] -- [1,2,3]
t3 = takeWhile (< 0) [1,2,3] -- []
--main = print ([t1, t2, t3]) -- [[1,2],[1,2,3],[]]


-- dropWhile returns the suffix remaining after takeWhile p xs
-- dropWhile drops the elements that satisfy the property
-- dropWhile :: (a -> Bool) -> [a] -> [a]

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p [] = []
dropWhile1 p (x : xs)
  | p x = dropWhile1 p xs
  | otherwise = (x : xs)

--main = print (dropWhile1 ff [1..10]) -- [1,2,3,4,5,6,7,8,9,10]

d1 = dropWhile (< 3) [1,2,3,4,5,1,2,3] -- [3,4,5,1,2,3]
d2 = dropWhile (< 9) [1,2,3] -- []
d3 = dropWhile (< 0) [1,2,3] -- [1,2,3]
--main = print ([d1, d2, d3]) -- [[3,4,5,1,2,3],[],[1,2,3]]

-- comparison
fi = filter even [1..10] -- [2,4,6,8,10]
tw = takeWhile even [2,4,6,7,8,9] -- [2, 4, 6]
dw = dropWhile even [2,4,6,7,8,9] -- [7, 8, 9]
-- main = print ([fi, tw, dw]) -- [[2,4,6,8,10],[2,4,6],[7,8,9]]


