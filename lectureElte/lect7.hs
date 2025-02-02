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
