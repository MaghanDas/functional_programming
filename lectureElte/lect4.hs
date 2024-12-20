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

-- which is _functionally_ equivalent to

-- main = print (map (\x -> x * 2) [1, 2, 3]) -- [2, 4, 6]

-- foldr -- applied to a binary operator,
-- a starting value (typically the right-identity of the operator),
-- and a list, reduces the list using the binary operator, from right to left:
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

-- foldr works just like foldl.
-- There are two differences.
-- The accumulator is the second argument to the function/operator argument.
-- Folding is done starting from the last element of a list.

f4 = foldr (+) 4 [1, 2, 3] -- 10
-- main = print (f4)
-- (1 + (2 + (3 + 4)))
-- (1 + (2 + 7))
-- (1 + 9) = 10

f5 = foldr (+) 3 [1 .. 5] -- 18
-- main = print (f5)
-- (1 + (2 + (3 + (4 + (5 + 3))))
-- (1 + (2 + (3 + (4 + 8))))
-- (1 + (2 + (3 + 12)))
-- (1 + (2 + 15))
-- (1 + 17) = 18

-- The difference between foldl and foldr can be explained with the following example.

fl = foldl (-) 0 [1, 2, 3, 4, 5] -- -15
-- main = print (fl)
-- (((((0 - 1) - 2) - 3) - 4) - 5)
-- ((((-1 - 2) - 3) - 4) - 5)
-- (((-3 - 3) - 4) - 5)
-- ((-6 - 4) - 5)\\

-- (-10 - 5) = -15

fr = foldr (-) 0 [1, 2, 3, 4, 5] -- 3
-- main = print (fr)
-- (1 - (2 - (3 - (4 - (5 - 0))))
-- (1 - (2 - (3 - (4 - 5))))
-- (1 - (2 - (3 - (-1))))
-- (1 - (2 - (3 + 1)))
-- (1 - (2 - 4))
-- (1 + 2) = 3

-- main = print (foldr (+) 100 [1..4]) -- 110

-- main = print (foldr (+) 100 [0]) -- 100

-- main = print (foldr (-) 0 [1..4]) -- -2

-- main = print [foldr (&&) True [True, False, True, True], foldr (||) False [False, False, True, True]] -- [False,True]

-- Scanning functions
-- scanl is similar to foldl, but returns a list of successive reduced values from the left
-- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]

-- main = print (scanl (+) 0 [1..4]) -- [0,1,3,6,10]
-- [0 , 0+1, 0+1+2, 0+1+2+3, 0+1+2+3+4]

-- main = print (scanl (+) 42 []) -- [42]

-- main = print (scanl (-) 100 [1..4]) -- [100,99,97,94,90]
-- [100 , 100-1, 100-1-2, 100-1-2-3, 100-1-2-3-4]

-- main = print (scanl (\x nextChar -> nextChar : x) "foo" ['a', 'b', 'c', 'd'])
-- ["foo","afoo","bafoo","cbafoo","dcbafoo"])

-- main = print (take 10 (scanl (+) 0 [1..]))
-- [0,1,3,6,10,15,21,28,36,45])

-- scanr - is the right-to-left dual of scanl.
-- the order of parameters on the accumulating function are reversed compared to scanl

-- main = print (scanr (+) 0 [1..4]) -- [10,9,7,4,0]
-- (1 + (2 + (3 + (4 + 0))) -- [10,9,7,4,0]

-- main = print (scanr (+) 42 []) -- [42]

-- main = print (scanr (-) 100 [1..4]) -- [98,-97,99,-96,100]
-- (1 - (2 - (3 - (4 - 100))) -- [98,-97,99,-96,100]

-- main = print (scanr (\nextChar x -> nextChar : x) "foo" ['a', 'b', 'c', 'd'])
-- ["abcdfoo","bcdfoo","cdfoo","dfoo","foo"]

-- Iterative functions
-- iterate - applies a function to a value infinitely many times
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x returns an infinite list of repeated applications of f to x
-- iterate f x == [x, f x, f (f x), ...]

iterate1 :: (a -> a) -> a -> [a]
-- iterate1 f x = x : iterate1 f (f x)
iterate1 f x = x:iterate1 f (f x)

-- main = print (take 10(iterate1 (+1) 10)) -- [10,11,12,13,14,15,16,17,18,19]
-- main = print(take 5 (iterate1 (+1)10))
-- main = print (take 10 (iterate (+3) 42)) -- [42,45,48,51,54,57,60,63,66,69]

-- main = print (take 10 (iterate id 1)) -- [1,1,1,1,1,1,1,1,1,1]

-- main = print (take 10 (iterate not True))
-- [True,False,True,False,True,False,True,False,True,False]

-- Tuples
{-
A tuple is a collection of fixed number of items,
as opposed to a list, which can contain any number of items - or even zero items.
Items in a tuple may be of different types,
as opposed to a list, where every item in the list must be of the same type.
A tuple with 2 elements is a completely different type from a tuple with 3 elements,
and so on.

Tuples are written within parentheses ()
and the elements of a tuple are separated by commas.
Example:
('a', 'b') :: (Char, Char)
('a', True) :: (Char, Bool)
('a', 'b', True, False) :: (Char, Char, Bool, Bool)

Returning multiple values from functions

Whenever you want to return multiple values from a function
where the type and number of return-values is known at the time of writing the code
(compile time, as opposed to run-time).

Ad-hoc way to “associate” elements

A lot of times you need a way to temporarily “associate” one piece of data
with another piece to achieve the larger goal of the app.

Examples: phonebook - name and the contact lists (String, [String]) – a list of 2-tuples.

("Mary", ["Ana", "Leo", "John", "Peter"])

-alphabet and list of contact names [(Char, [String])] – a list of 2-tuples.

[ ('a', ["Adam", "Anurag", "Ashtong"])
, ('b', ["Bhushan", "Bimal", "Banksky", "Beth"])
, ('c', ["Charlie", "Chaman", "Chilgoza"])
]

Passing multiple values to a lambda which accept only a single parameter
and so on ...
-}

-- fst ::	(a,b) -> a
-- returns the first item in a tuple

fst1 :: (a, b) -> a
fst1 (x, y) = x

-- main = print (fst1 (1,2), fst1 (1,(2,3,4)), fst1 ('a', "alma") ) -- (1,1,'a')

-- snd ::	(a,b) -> b
-- returns the second item in a tuple

-- snd1 :: (a, b) -> b
-- snd1 (x, y) = x 

-- main = print(snd1(1,2))
-- main = print (snd1 (1,2), snd1 (1,(2,3,4)), snd1 ('a', "alma") ) -- (2,(2,3,4),"alma")

-- Zipping
-- zip -  takes two lists and returns a list of corresponding pairs.
-- zip is right-lazy:
-- zip :: [a] -> [b] -> [(a, b)]

-- z1 = zip [1,  2] ['a', 'b', 'c'] -- [(1,'a'),(2,'b'),(3,'c')]
-- main = print z1

-- If one input list is shorter than the other,
-- excess elements of the longer list are discarded,
-- even if one of the lists is infinite:

z2 = zip [1] ['a', 'b'] -- [(1,'a')]
-- main = print (z2)

z3 = zip [1, 2] ['a'] -- [(1,'a')]
-- main = print (zip3)

z4 = zip [] [1 ..] -- [] error
-- main = print (z4)

-- z5 = zip [1 ..] [] -- []
-- main = print (z5)

-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
-- zip3 takes three lists and returns a list of triples, analogous to zip.

z6 = zip3 [1 .. 5] [20 .. 30] [100 .. 110]

-- main = print (z6)
-- [(1,20,100),(2,21,101),(3,22,102),(4,23,103),(5,24,104)]

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function.
-- zipWith (,) xs ys == zip xs ys
-- zipWith f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]

z7 = zipWith (+) [1, 2, 3] [4, 5, 6] -- [5,7,9]
-- main = print (z7)

z8 = zipWith (++) ["hello ", "foo"] ["world!", "bar"]

-- main = print (z8)
-- ["hello world!","foobar"]

-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- the zipWith3 function takes a function which combines three elements,
-- as well as three lists and returns a list of the function applied
-- to corresponding elements, analogous to zipWith.
-- zipWith3 (,,) xs ys zs == zip3 xs ys zs
-- zipWith3 f [x1,x2,x3..] [y1,y2,y3..] [z1,z2,z3..] == [f x1 y1 z1, f x2 y2 z2, f x3 y3 z3..]

z9 = zipWith3 (\x y z -> [x, y, z]) "123" "abc" "xyz"

-- main = print (z9)
-- ["1ax","2by","3cz"]

z10 = zipWith3 (\x y z -> (x * y) + z) [1, 2, 3] [4, 5, 6] [7, 8, 9]

-- main = print (z10)

-- [1*4+7, 2*5+8, 3*6+9] = [11,18,27]