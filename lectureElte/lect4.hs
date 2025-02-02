-- import Main (toDigit)
main :: IO ()
-- Higher order functions - review

-- map - applies a function f to each element of a list
-- map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- map :: (a->b) -> [a] -> [b]

map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x : xs) = f x : map1 f xs

fm :: Int -> Int
fm x = 2 * x

-- main = print (map fm [1..10]) --[2,4,6,8,10,12,14,16,18,20]

fm2 :: Int -> Float
fm2 x = fromIntegral x / 2

-- main = print (map fm2 [1..10]) -- [0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]

-- Lambda expressions

-- An anonymous function is a function without a name.
-- It is a lambda abstraction and might look like this: \x -> x + 1

-- la = (+ 1) 4

-- main = print (la) -- 
-- this a nameless function of two parameters, x and y
la2 = (\x y -> x + y) 3 5

-- main = print (la2) -- 8

-- this a nameless function to add 10 to the argument x
la3 = map (\x -> 10 + x) [1 .. 5]

-- main = print (la3) -- [11,12,13,14,15]

-- the above can be rewritten in "pointfree style" or with partial parameters
-- (\x -> 10 + x) is equivalent to  (+10)
la4 = map (+ 10) [1 .. 5]

-- main = print (la4) -- [11,12,13,14,15]

lm1 = map (+ 1) [1, 2, 3] -- [2,3,4]

lm2 = map id [1, 2, 3] -- [1,2,3] -- id x = x

lm3 = map (\n -> 3 * n + 1) [1, 2, 3] -- [4,7,10]
-- main = print ([lm1, lm2, lm3]) -- [[2,3,4],[1,2,3],[4,7,10]]

-- filter - applied to a predicate and a list,
-- returns the list of those elements that satisfy the predicate
-- filter :: (a -> Bool) -> [a] -> [a]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x : xs)
  | p x = x : filter1 p xs
  | otherwise = filter1 p xs

ff :: Int -> Bool
ff x = x `rem` 2 == 0

-- main = print (ff 23) -- False
-- main = print (filter1 ff [1..10]) -- [2,4,6,8,10]
-- main = print (filter1 ff [3,5,7,2,9]) -- [2]

fil1 = filter even [1, 2, 3] -- [2]
-- main = print (fil1)

-- fil2 = filter (\l -> length l > 3) ["Hello", ", ", "World", "!"] -- ["Hello","World"]

fil3 = filter (/= 3) [1, 2, 3, 4, 3, 2, 1] -- [1,2,4,2,1]
-- main = print (fil3)

-- takeWhile - takes the elements of the list that satisfy the property
-- takeWhile :: (a -> Bool) -> [a] -> [a]

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x : xs)
  | p x = x : takeWhile1 p xs
  | otherwise = []


-- main = print (takeWhile1 ff [1..10]) -- []
-- main = print (takeWhile1 odd [1..10]) -- [1]

t1 = takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4] -- [1,2]

t2 = takeWhile (< 9) [1, 2, 3] -- [1,2,3]

t3 = takeWhile (< 0) [1, 2, 3] -- []
-- main = print ([t1, t2, t3]) -- [[1,2],[1,2,3],[]]

-- dropWhile - drops the elements that satisfy the property
-- dropWhile :: (a -> Bool) -> [a] -> [a]

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p [] = []
dropWhile1 p (x : xs)
  | p x = dropWhile1 p xs
  | otherwise = x : xs

-- main = print (dropWhile1 ff [1..10]) -- [1,2,3,4,5,6,7,8,9,10]

d1 = dropWhile (< 3) [1, 2, 3, 4, 5, 1, 2, 3] -- [3,4,5,1,2,3]

d2 = dropWhile (< 9) [1, 2, 3] -- []

d3 = dropWhile (< 0) [1, 2, 3] -- [1,2,3]
-- main = print ([d1, d2, d3]) -- [[3,4,5,1,2,3],[],[1,2,3]]

-- comparisons
fi = filter even [1 .. 10] -- [2,4,6,8,10]

tw = takeWhile even [2, 4, 6, 7, 8, 9] -- [2, 4, 6]

dw = dropWhile even [2, 4, 6, 7, 8, 9] -- [7, 8, 9]
-- main = print ([fi, tw, dw]) -- [[2,4,6,8,10],[2,4,6],[7,8,9]]
-- end review

-- More lambda examples:

-- simple expressions
-- la5 = map(\x -> x* 5)[1,2,3]
-- main = print (la5)

-- argument can replace a function
-- -- la6 = (\x -> x drop 2) [11,2,3,4] -- 11
-- la6 :: [a] -> [a]
-- la6 = take 2
-- main = print (la6[1,2,3])

-- multiple arguments
-- la7 = (\x y z-> x * y * z) 2 10 5 -- 100



-- usage example
la8 = filter (\x -> x `rem` 3 == 0) [2, 18, 9, 22, 17, 24, 8, 12, 27] -- [18, 9, 24, 12, 27]
even1 = filter (\x -> even x) [2, 18, 9, 22, 17, 24, 8, 12, 27]
-- main = print even1
-- main = print (la8) 


-- Folding
-- foldl - a higher order function that takes three arguments.
-- The first one is a function / operator with 2 arguments.
-- The second argument is called the accumulator.
-- The third argument is the list.
-- foldl - folds left associatively with the help of
-- the function argument and the accumulator.
-- foldl, applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a list,
-- reduces the list using the binary operator, from left to right:

-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- The list must be finite.

f1 = foldl (+) 4 [1, 2, 3] -- 10
-- main = print (f1)
-- (((4 + 1) + 2) + 3)
-- ((5 + 2) + 3)
-- (7 + 3) = 10

f2 = foldl (+) 3 [1 .. 5] -- 18
-- main = print (f2)
-- (((((3 + 1) + 2) + 3) + 4) + 5)
-- ((((4 + 2) + 3) + 4) + 5)
-- (((6 + 3) + 4) + 5)
-- ((9 + 4) + 5)
-- (13 + 5) = 18

f3 = foldl (++) "hello " ["my ", "friend, ", "how ", "are ", "you ", "doing?"]

-- main = print f3
-- -- "hello my friend, how are you doing?"

-- more foldl exmples:

-- main = print [foldl (+) 0 [1..4], foldl (+) 42 [], foldl (-) 100 [1..4]] -- [10,42,90]

-- main = print (foldl (+) 10 [1..4]) -- 20

-- main = print (foldl (&&) True [True, False, True, True]) -- False

-- main = print (foldl (||) False [False, False, True, True]) -- True

-- main = print (foldl (+) 0 [1..]) -- error * Hangs forever *

-- main = print (foldl (\total x -> total + x) 5 [1, 2, 3, 4]) -- 15

-- Doubling all elements of a list: rewriting map with foldl

-- main = print (foldl (\result x -> result ++ [x * 2]) [] [1, 2, 3]) -- [2, 4, 6]

