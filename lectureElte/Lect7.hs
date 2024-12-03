{-# LANGUAGE ParallelListComp #-}

import Data.Char (toUpper)
import Data.List

main :: IO ()

-- Generators - list comprehension -- review

s :: String
s = "Hello"
g1 = [toUpper c | c <- s] 
--main = print g1 -- "HELLO"

-- double all elements

g2 = [ 2*x | x <- [1..10] ]
--main = print g2 -- [2,4,6,8,10,12,14,16,18,20]


g3 = [ (3*x, x*x, x^3) | x <- [1..5] ]
--main = print g3 
-- [(3,1,1),(6,4,8),(9,9,27),(12,16,64),(15,25,125)]


-- multiple generators, separated by commas 
-- Cartesian product (Descartes product) from sets A x B

g4 = [ (i,j) | i <- [1,2], j <- [1..4] ]
--main = print g4
-- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]


-- each successive generator refines the results of the previous generator
-- if the second list is infinite, one will never reach 
-- the second element of the first list

g5 = take 10 [ (i,j) | i <- [1,2], j <- [1..] ]
--main = print g5
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]


-- in such a situation, a nested sequence of list comprehensions may be appropriate

g6 = take 5 [ [ (i,j) | i <- [1,2] ] | j <- [1..] ]
--main = print g6
-- [[(1,1),(2,1)], [(1,2),(2,2)], [(1,3),(2,3)], [(1,4),(2,4)], [(1,5),(2,5)]]


-- we can also provide Boolean guards

g7 = [ (x,y) | x<-[1..4],  y<-[1..5], y<=x ] 
--main = print g7
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)]


g8 = [ (i,j) | i <- [1..5], j <- [1..i-1],  even j ]
--main = print g8
-- [(3,2),(4,2),(5,2),(5,4)]


g9 = take 10 [ (i,j) | i <- [1..], j <- [1..i-1], gcd i j == 1 ]
--main = print g9
-- [(2,1),(3,1),(3,2),(4,1),(4,3),(5,1),(5,2),(5,3),(5,4),(6,1)]


g10 = [ (w,x) | w <- [1..4] , x <- [1..4], w /= x ]
--main = print g10
-- [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]

-- end review


-- List comprehension general form
-- [ <output function> | <input set>, ..., <predicate>, ... ]
-- it is map and filter equivalent


-- square of even numbers from 1 to 10
l1 = [ x*x | x <- [1..10], even x ]
--main = print l1
-- [4,16,36,64,100]


-- multiple input will loop over every possible pair
l2 =  [ (2*x,3*y) | x <- [1..3], y <- [4..6] ]
--main = print l2
-- [(2,12),(2,15),(2,18),(4,12),(4,15),(4,18),(6,12),(6,15),(6,18)]


-- multiple predicates will only return values which satisfy all of the predicates
l3 = take 10 [ x | x <- [1..], x > 10, x /= 21, odd x ]
--main = print l3
-- [11,13,15,17,19,23,25,27,29,31]


-- FizzBuzz 
l4 = [ if (x `mod` 15 == 0) then "FizzBuzz" else if (x `mod` 3 == 0) then "Fizz" else if (x `mod` 5 == 0) then "Buzz" else show x | x <- [1..18] ]
--main = print l4
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz"]


-- extract vowels from a String
-- remember that strings in Haskell are just lists of characters
l5 = [ x | x <- "The quick brown fox jumps over the lazy dog.", x `elem` "aeiouy" ]
--main = print l5
-- "euioouoeeayo"


-- real-world example, filtering with multiple predicate
-- only meals that satisfy both conditions will be kept

meals = [("Burger", 3, 310), ("Pizza", 5, 340), ("Ramen", 2, 250)]
filteredMeals = [name | (name, price, calories) <- meals, price < 4, calories > 300]
-- main = print filteredMeals
-- ["Burger"]


-- Prime Numbers

-- check if x is divisible by n
divisible :: Int -> Int -> Bool
divisible x n = rem x n == 0

-- collect divisors of a number in a list
divisors :: Int -> [Int]
divisors x = filter (divisible x) [1..x]

-- check if a number is prime
-- i.e. equal to the list of 1 and the number itself
prime :: Int -> Bool
prime x = divisors x == [1,x]
 
-- create primes up to a given number
primes :: Int -> [Int]
primes x = filter prime [1..x]

primeUpTo100 = primes 100 
--main = print primeUpTo100
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]


-- Sieve of Eratosthenes - generating primes
sieve :: [Int] -> [Int]
sieve (p:xs) = (p : sieve [ x | x <- xs, rem x p /= 0])

-- generate first 100 primes
primes100 = take 100 (sieve [2..])
--main = print primes100
-- [2,3,5,7,11,13,17,19,23,29,31 ..] -- first 100 pieces of primes

-- sieve [2,3,4,5,6,7,8,..]
-- 2:sieve [x | x <-[3,4,5,...], rem x 2 /= 0] 
-- 2:sieve [3,5,7,9,11,13,15,...]
-- 2:3:sieve [x | x<-[5,7,11,13,17,19,...],  rem x 3 /= 0]
-- 2:3:sieve [5,7,11,13,17,19,...]
-- ...
-- 2:3:5:sieve [7,11,13,17,19,...]
-- ... until it gets the first 100 pieces of primes


-- Mersenne primes
-- find Mersenne primes (primes of the form 2^n - 1):
mersenne =  [p | p <- primes100, powerOf2 (p+1)]

powerOf2 :: Int -> Bool
powerOf2 1 = False
powerOf2 2 = True
powerOf2 n = even n && powerOf2 (n `div` 2)

--main = print mersenne
--[3,7,31,127]


-- patterns in generators
-- extract heads of sublists
l62 = [ head x | x<-[[3],[1,2],[],[4,5,6],[],[10]] ]
l6 = [ x | (x:xs)<-[[3],[1,2],[],[4,5,6],[],[10]] ]
--main = print l6
-- [3,1,4,10]


-- compute x^(x+2) where x is even from [1..10] 
l71 = [ x^(x+2) | x <- [1..10], even x ]
l7 = [ x^y | (x,y) <- zip [1..10] [2,4..], even x ]
--main = print l7
-- [16,65536,2176782336,281474976710656,100000000000000000000]


l8 = [(+) x y |x<-[1..3],y<-[3,7]]
--main = print l8
-- adds to 1 the 3 and then 7, then adds to 2 the 3 and then 7 
-- then adds to 3 the 3 and then 7
-- [4,8,5,9,6,10]


-- would be more complicated without list comprehensions
addLists :: [Int]->[Int]->[Int]
addLists list1 [] = []
addLists list1 (a:x) = addListElem list1 a ++ addLists list1 x

addListElem :: [Int]->Int->[Int]
addListElem [] a = []
addListElem (b:x) a = (b+a):addListElem x a

--main = print (addLists [1..3] [3,7])
-- [4,5,6,8,9,10]


-- parallel list comprehension
-- a parallel list comprehension has multiple independent branches of lists, 
-- each separated by a | symbol
-- the behaviour of parallel list comprehensions follows that of a zip function, 
-- the resulting list will have the same length as the shortest branch
-- it needs the pragma {-# LANGUAGE ParallelListComp #-}

l9 = [ (x, y) | x <- [1..10] | y <- [100..] ]
--main = print l9
-- [(1,100),(2,101),(3,102),(4,103),(5,104),(6,105),(7,106),(8,107),(9,108),(10,109)]


-- l7 = [ x^y | (x,y) <- zip [1..10] [2,4..], even x]
l72 = [ x^y | x <- [2,4..10] | y <- [4,8..] ]
--main = print l72
-- [16,65536,2176782336,281474976710656,100000000000000000000]


-- permutations - recursion in generator
-- [[1,2,3,4,5],[1,2,3,5,4],[1,2,4,3,5],[1,2,4,5,3], ... [5,4,3,2,1]]

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms x = [ a:p | a <- x, p <- perms (filter (\y -> y /= a) x) ]

--main = print (perms [1..3])
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] -- 3! pieces

-- using \\ of Data.List
-- The \\ function is list difference (non-associative). 
-- In the result of xs \\ ys, the first occurrence of each element of ys 
-- is removed from xs
-- Thus (xs ++ ys) \\ xs == ys

perms2 :: [Int] -> [[Int]]
perms2 [] = [[]]
perms2 x = [ a:p | a <- x, p <- perms2 (x\\[a]) ]

--main = print (perms2 [1..3])
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]


-- Fibonacci sequence: 1,1,2,3,5,8,13,21,...
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
   
slowFibSeq = take 10 [fib n | n <- [1..]]
--main = print slowFibSeq
-- [1,1,2,3,5,8,13,21,34,55]


fib2 :: Int -> Int
fib2 n = fibAux n 1 1

fibAux :: Int -> Int -> Int -> Int
fibAux 1 a b = a
fibAux i a b 
  | i > 1 = fibAux (i-1) b (a+b)

betterFibSeq = take 10 [fib2 n | n <- [1..]]
--main = print betterFibSeq
-- [1,1,2,3,5,8,13,21,34,55]


-- more efficient computation
fibSeq = 1 : 1 : [ f | f <- zipWith (+) fibSeq (tail fibSeq)]
nFib n = fibSeq  !!  n
--main = print (take 10 fibSeq) 
-- [1,1,2,3,5,8,13,21,34,55]


fibSeq2 = 1 : 1 : [ x+y | (x,y) <- zip fibSeq2 (tail fibSeq2) ]
--main = print (take 10 fibSeq2)
-- [1,1,2,3,5,8,13,21,34,55]

fibSeq3 = 1 : 1 : [ x+y | x <- fibSeq3 | y<- (tail fibSeq3)]
--main = print (take 10 fibSeq3)
-- [1,1,2,3,5,8,13,21,34,55]


-- Sorting
-- sort function from module Data.List
sorted = sort [1,3,5,2,4,1]
--main = print sorted
-- [1,1,2,3,4,5]


-- Quicksort using list comprehension
qsort  []     =  []
qsort (x:xs)  =  qsort [y | y <- xs, y<x ] ++ [x] ++ qsort [y | y <- xs, y>=x]

main = print (qsort [2,1,5,6,9,0,1])
-- qsort [1,0,1] ++  [2]  ++ qsort [5,6,9]
-- qsort [0] ++[1]++ qsort [1]   ++[2]++   qsort [] ++[5]++ qsort [6,9]
-- qsort [] ++[0]++ qsort [] ++[1]++ qsort [] ++[1]++ qsort []  ++[2]++ [] ++[5]++ qsort [] ++[6]++ qsort [9]
-- []++[0]++[] ++[1]++ []++[1]++[] ++[2]++ []++[5]++[] ++[6]++ qsort [] ++[9]++ qsort []
-- []++[0]++[] ++[1]++ []++[1]++[] ++[2]++ []++[5]++[] ++[6]++ []++[9]++[]
-- [0,1,1,2,5,6,9]
