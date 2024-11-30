{-# OPTIONS_GHC -Wno-empty-enumerations #-}
import System.Win32 (COORD(xPos))
import Control.Arrow (Arrow(first))
import Distribution.FieldGrammar (uniqueField)

main :: IO ()
-- Higher order function foldr, foldl, dropWhile, scan, iterate

-- 1.  Write a function that calculates the sum of all numbers in a list that are divisible by 3 and 5 using a foldl and filter function.
hof1 :: [Int] -> Int
hof1 ls =foldr (+) 0 (filter (\x -> rem x 3 ==0 && rem x 5==0) ls)

-- main = print(hof1 [1..20]) -- 15
-- main = print(hof1 [1..100]) -- 315

-- 2. Given a list of Strings, write a function that concatenates them all together with a space between them.
--    Hint: use foldr.
hof2 :: [String] -> String
-- hof2 = foldr(++) " "
hof2 = foldr (\x y -> x ++ " "++y) ""
-- main = print(hof2 ["Hello", "world"]) -- "Hello world"
--main = print(hof2 ["I" , "am", "a", "programmer"]) -- "I am a programmer"

-- 3. Given a list of Integers. Write a function that drops elements while the element is positive.
--    Hint: use dropWhile.
hof3 :: [Int] -> [Int]
hof3 = dropWhile (>=0)
-- main = print(hof3 [10,9..(-10)]) -- [-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

-- 4. Write a function to print first 10 even numbers starting from 0.
--    Use take and iterate.
-- iterate (+2)
hof4 :: [Int]
hof4 = take 10 (iterate (+2) 0)
-- main = print(hof4) -- [0,2,4,6,8,10,12,14,16,18]

-- 5. Write a function that generates the list of all powers of 2.
--    Use iterate.
hof5 :: [Int]
hof5 = [x^2 | x <- [1..] ,2`mod`x==0]
-- main = print (take 10 hof5) -- [1,2,4,8,16,32,64,128,256,512]

-- 6. Given a list of Integers. Use scanl to calculate the sum of all elements.
hof6 :: [Int] -> [Int]
hof6 = scanl (+) 0
-- main = print (hof6 [1..10]) -- [0,1,3,6,10,15,21,28,36,45,55]

-- Tuple zip, zipWith

-- 1. Given a tuple of two, write a function that swaps the elements of the tuple.
swap :: (a, b) -> (b, a)
swap (a,b) = (b,a)
-- main = print(swap (1, 2)) -- (2,1)

-- 2. Given a tupple of three elements, write three function that extract the first, second, and third element of the tuple.
fstTriple :: (a, b, c) -> a
fstTriple(a,b,c) = a

sndTriple :: (a, b, c) -> b
sndTriple(a,b,c) = b

thdTriple :: (a, b, c) -> c
thdTriple(a,b,c) = c
t = (1,2,3)

-- main = print(fstTriple t) -- 1
-- main = print(sndTriple t) -- 2
--main = print(thdTriple t) -- 3


-- 3. Given a list of alphabet letters and a list of integers, write a function that pairs them up.
-- t1 :: [Int] -> [Char] -> [(Int, Char)]
-- t1  ls1 ls2 = zipWith(\x y -> (x,y)) ls1 ls2
-- main = print(t1 [1..] ['a'..'z']) -- [(1,'a'),(2,'b'),(3,'c'),...,(26,'z')]


-- 4. Given two lists of Integers of the same length.
--    Write a function that pairs them up and computes the maximum of each pair.

t2 :: [Int] -> [Int] -> [Int]
t2 = zipWith (\x y -> max x y)
-- t2 = zipWith max

-- main = print (t2 [1..10] [10,9..1]) -- [10,9,8,7,6,6,7,8,9,10]

-- List comprehensions
-- 1. Generate a list with 10 times of 13: [13,13,13,13,13,13,13,13,13,13]
l1 :: [Int]
l1 = [13 | x<-[1..10]]
-- main = print(l1)


-- 2. Generate the following list [(1,1),(1,2),(2,1),(2,2)]
l2 :: [(Int, Int)]
l2 = [(x,y) | x<-[1,2],y<-[1,2]]
-- main = print(l2)


-- 3. Generate the following list [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
l3 :: [(Int, Int)]
l3 = [(x,y) | x<-[1,2,3],y<-[3,2,1]]
-- main = print l3

-- 4. Generate the list [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
l4 :: [(Int, Int)]
l4 = zip [1..6][5..10]
-- main = print(l4)
-- 5. Generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]

l5::[Int]
l5 = concat[replicate x x | x<-[1..10]]
-- main = print(l5)
-- main = print(l51)
-- main = print(l52)


-- 6. Generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
-- replicate
-- Generate the list [[1],[2,2],[3,3,3],...[10,10,...,10]]
l6 :: [[Int]]
l6 = [replicate x x | x<-[1..10]]
-- main = print l6


-- 7. Generate 100 Pythagorean triples : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
-- l7 :: [(Int, Int, Int)]

-- -- just c is restricted, not the number of pairs
-- l71 = [ (a,b,c) | c <- [1..100], b<-[1..c], a<-[1..b] , a*a + b*b == c*c ]

-- -- a,b,c restricted but not the nr of pairs
l72 =  take 100 [ (a,b,c) | a <- [1..100], b<-[1..100], c <- [1..100] , a*a + b*b == c*c && a<b && b<c ]

-- main = print(l7)
--main = print(l71)
-- main = print(l72)


-- 8. Generate 100 even numbers using list comprehensions
l8 :: [Int]
l8 = [x | x <-[1..100], even x ]
-- l82 :: [Int]
-- main = print(l8)
--main = print(l82)

-- 9. Generate the following list [4, 16, 36, 64, 100, 144, 196, 256, 324, 400]
l9 :: [Int]
l9 = [x^2 | x <- [2..20], even x]
-- main = print l9



---- List powers of 2 from 1 to 10
powersOf2 :: [Int]
powersOf2 = [2^x | x <- [1..10]]

-- main = print powersOf2

-- 11. List the divisors of 90.
l11 :: [Int]
l11 = [ x|x<-[1..90], 90`rem` x == 0 ]
-- main = print(l11)

-- 12. List dominoes: [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),...(9,9)]
-- Domino (1,0) is not in the list because it is already in it as (0,1).
l12 :: [(Int, Int)]
l12 = [(x,y) | x<-[0..9], y<-[x..9]]
-- main = print(l12)


-- 13. Construct the list [(1,'a'),(2,'b'),...(26,'z')], i.e. pair up numbers with abc letters.
l13 :: [(Int, Char)]
l13 = zip [1..26] ['a'..'z'] 
-- main = print(l13)


-- 14. Generate a list of length 10 whose elements are False, True, False, True... (alternating)
l14 :: [Bool]
l14 = take 10 (cycle [False, True])
-- l14 = [x `mod` 2 == 0 | x <- [1..10]]
-- main = print l14


-- 15. Is 123457 a prime number? A number is prime if only 1 and the number divides it.
l15 :: Int->Bool
l15 n = length[x | x<- [1..n], n`mod`x==0]==2
-- main = print(l15 9)


-- 16. Generate the list [(0,10),(1,9),(2,8),...(10,0)]
l16 :: [(Int, Int)]
l16 = zip[0..10] (reverse [0..10])
-- main = print(l16)

-- 17. Generate a list that contains all (hour, minute) pairs in a day.
-- l17 :: [(Int, Int)]

-- main = print(l17)


-- 18. Generate the following list [(1,1),(2,2),(3,3),(4,4),(5,5)]
--l18 :: [(Int, Int)]

--main = print(l18)


-- 19. Generate [(1,2,3),(2,4,6),(3,6,9),(4,8,12),(5,10,15)]
--l19 :: [(Int, Int, Int)]

--main = print(l19)


-- 20. Compute the sum of the list of tuples [(1,1), (2,2), (3,3)] -> (6,6)
-- unzip
--sumtup :: [(Int, Int)] -> (Int, Int)

--main = print(sumtup [(1,1), (2,2), (3,3)])


-- 21. Generate 5 tuples like [(1,2),(2,3),(3,4),(4,5),(5,6)]
--increase :: [(Int, Int)]

--main = print(increase)


-- 22. Make triple tuples like [(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)]
--tripl :: [(Int, Int, Int)]

--main = print(tripl)


-- 23. Given a list of lists, transform it into tuples of sublists such that two continuous sublists form pairs
-- (if there are odd number of sublists the last has as pair the empty list)

--pairs :: [[Int]] -> [([Int], [Int])]

--main = print(pairs [[1,2,3], [5,6], [7,8,9,10], [11,3], [1..5]])
--main = print(pairs [[1,2,3], [5,6], [7,8,9,10], [11,3]])


-- 24. Given a list of tuples form a list of triple tuples with the original numbers and their sum
--triplesum :: [(Int, Int)] -> [(Int, Int, Int)]

--main = print(triplesum [(1,2),(2,3),(3,4),(4,5),(5,6)])


-- 25. Generate quadruples of a number, its square, its cube, and its biquadratic (power 4)
-- where the number are in the 1..20 interval

--quadruple :: [(Int, Int, Int, Int)]

--main = print(quadruple)


-- 26. Form triple tuples of 3 lists selecting one element from each list.
-- You must use list comprehension.
-- E.g. for ([1..10],[20..25],[35..47]) the result is 
-- [(1,20,35),(2,21,36),(3,22,37),(4,23,38),(5,24,39),(6,25,40)]

-- Extract the first of a triple.
--fst3 :: (a,b,c) -> a

-- Extract the second of a triple.
--snd3 :: (a,b,c) -> b

-- Extract the third element of a triple.
--thd3 :: (a,b,c) -> c

--tri :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]

--tri2 :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]

-- tri3 :: ([Int], [Int], [Int]) -> [(Int, Int, Int)]
-- tri3 (a,b,c) = zip3 a b c

--main = print(tri ([1..10],[20..25],[35..47]))
--main = print(tri2 ([1..10],[20..25],[35..47]))
--main = print(tri3 ([1..10],[20..25],[35..47]))


-- 27. Write a function duplicates which checks if there are neighbor duplicates in a list.
--duplic :: [Int] -> Bool

--main = print(duplic [1, 1]) -- True
--main = print(duplic [2]) -- False
--main = print(duplic [1, 2, 3, 4, 5, 6, 7, 8, 9]) -- False
--main = print(duplic [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0]) -- True
--main = print(duplic [1, 2, 3, 4, 4]) -- True


-- 28. Write a function that removes neighbor duplicates in a list.
--duplicRem :: [Int] -> [Int]

--main = print(duplicRem [1, 1, 0, 5, 0, 0, 6, 0, 0, 0, 7, 5, 0, 0, 0, 0, 8, 0, 5, 0, 0, 0]) 


-- 29. Transform the sub-sub lists into one list of sublists
--f :: [[[Int]]] -> [[Int]]

--main = print(f [[[1,2,3], [3,4,5]], [[1,2,3], [3,4,5], [7,8,9]]]) -- [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]


-- 30. Generate a list that contains all (month, day) pairs in a 365-day year.
-- you can consider the number of days in the following way
-- Jan -> 31 days, Feb -> 28 days (ignore leap year), Mar -> 31 days, Apr -> 30 days, 
-- May -> 31 days, Jun -> 30 days, Jul -> 31 days, Aug -> 31 days, Sep -> 30 days, 
-- Oct -> 31 days, Nov -> 30 days, Dec -> 31 days.
--l30 :: [(Int, Int)]

-- main = print(l30)