import Data.Char (toUpper)

main :: IO ()
-- continue by line 235
-- two practice tasks
-- 12. Instert 0 in front of every sublist of a list.
ins0 :: [[Int]] -> [[Int]]
-- ins0  = map ((++) [0])    -- \x = [0] ++ x
ins0 = map (0 :)

-- main = print(ins0 [[1,2,3],[5,6],[],[7,8,9,10]])
-- [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]

-- insert 0 at the end of evry sublist!!
ins0' :: [[Int]] -> [[Int]]
-- ins0' = map (++ [0])   -- \x = x ++ [0]
ins0' = map (++ [0])

-- main = print (ins0' [[1,2,3],[5,6],[],[7,8,9,10]])
-- [[1,2,3,0],[5,6,0],[0],[7,8,9,10,0]]

-- 21. Filter the elements smaller then n, e.g. n=3

f7 :: Int -> [Int] -> [Int]
-- f7 n  = filter (>n)  -- n > x
f7 n = filter (< n) -- x < n    both are good
-- main = print(f7 3 [1,5,3,2,1,6,4,3,2,1])  -- [1,2,1,2,1]

----------------

-- $  -- apply
-- `f $ x` means "apply `f` to `x`."
-- it is used to reduce the need for parentheses
-- useful for avoiding extra parentheses: f (g x) == f $ g x

-- $ is infix "application", it is defined as
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- or
-- ($) f x = f x
-- or
-- ($) = id   -- identity function id x = x

t = take 1 $ filter even [1 .. 10]

-- main = print t  -- [2]

-- What happens if we don't put the $?
-- take 1 filter even [1..10]
-- the compiler would now complain, because it would "think"
-- we are trying to apply 4 arguments to the take function, with the arguments being
-- 1 :: Int, filter :: (a -> Bool) -> [a] -> [a], even :: a -> Bool, [1..10] :: [Int].
-- is obviously incorrect, we could put parenthesis around our expression:
-- (take 1) (filter even [1..10])
-- This would reduce to:
-- (take 1) ([2,4,6,8,10])
-- which then becomes:
-- take 1 [2,4,6,8,10]
-- alternatively we can place the $ sign between where the pair of parenthesis would go
-- take 1 $ filter even [1..10]

-- .  -- compose
-- the `.` operator is function composition
-- `f . g` means "apply `g` first, then apply `f` to the result of `g`
-- it is f o g from maths
-- f . g . h $ x   can be  (f . g . h) x
-- (.) :: (b -> c) -> (a -> b) -> a -> c

add1 :: Int -> Int
add1 x = x + 1

double :: Int -> Int
double x = x * 2

-- Partial parameterization
-- Compose the functions
add1ThenDouble = double . add1

-- Usage
result = add1ThenDouble 3 -- 8 (3 + 1 = 4, then 4 * 2 = 8)
-- main = print (result)

-- Without $
result1 = double (add1 3) -- 8
-- main = print (result1)

-- With $
result2 = double $ add1 3 -- result is also 8, but cleaner
-- main = print (result2)

-- Use . for composing functions, creating a new function from existing ones.
-- Use $ to apply a function to an argument, helping to avoid parentheses in expressions.

-- tricky usage of $ "apply the argument to whatever function"

m = map ($ 10) [(+ 1), (+ 2), (+ 3)] -- +1 is applied to 10, then +2 to 10 and +3 to 10 [10+1,10+2,10+3]
-- main = print m -- [11, 12, 13]

-- m1 = map (($) 10) [(+1), (+2), (+3)] -- error not 10 applied to +1, +2, +3

-- undefined  -- exceptional value
-- it is an exceptional value that can be an element of any type
-- acts like a placeholder

u1 = head (100 : undefined)

-- main = print u1 -- 100

u2 = length [undefined, undefined, undefined, undefined]

-- main = print u2 -- 4

-- Review + new functions

-- foldl
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn

-- main = print [foldl (+) 0 [1..4], foldl (+) 42 [], foldl (-) 100 [1..4]] -- [10,42,90]

-- foldr
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

-- main = print [foldr (&&) True [True, False, True, True], foldr (||) False [False, False, True, True]] -- [False,True]

-- scanl
-- scanl is similar to foldl, but returns a list of successive reduced values from the left
-- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- note: last (scanl f z xs) == foldl f z xs

-- main = print (scanl (+) 0 [1..4]) -- [0,1,3,6,10]
-- [0 , 0+1, 0+1+2, 0+1+2+3, 0+1+2+3+4]

-- scanl1
-- is a variant of scanl that has no starting value argument
-- scanl1 :: (a -> a -> a) -> [a] -> [a]
-- scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

s1 = scanl1 (+) [1 .. 4] -- [1,3,6,10]
-- main = print s1

s2 = scanl1 (+) [] -- []
-- main = print s2

s3 = scanl1 (-) [1 .. 4] -- [1,-1,-4,-8]
-- main = print s3

s4 = scanl1 (&&) [True, False, True, True] -- [True,False,False,False]
-- main = print s4

s5 = scanl1 (||) [False, False, True, True] -- [False,False,True,True]
-- main = print s5

s6 = take 10 (scanl1 (+) [1 ..]) -- [1,3,6,10,15,21,28,36,45,55]
-- main = print s6

s7 = take 1 (scanl1 undefined ('a' : undefined)) -- "a"
-- main = print s7

-- scanr
-- scanr - is the right-to-left dual of scanl.
-- the order of parameters on the accumulating function are reversed compared to scanl
-- note: head (scanr f z xs) == foldr f z xs

-- main = print (scanr (+) 0 [1..4]) -- [10,9,7,4,0]
-- (1 + (2 + (3 + (4 + 0))) -- [10,9,7,4,0]

-- scanr1
-- scanr1 is a variant of scanr that has no starting value argument
-- scanr1 :: (a -> a -> a) -> [a] -> [a]

s8 = scanr1 (+) [1 .. 4] -- [10,9,7,4]
-- main = print s8

s9 = scanr1 (+) [] -- []
-- main = print s9

s10 = scanr1 (-) [1 .. 4] -- [-2,3,-1,4] = [1-3, 2-(-1), 3-4, 4]
-- main = print s10

s11 = scanr1 (&&) [True, False, True, True] -- [False,False,True,True]
-- main = print s11

s12 = scanr1 (||) [True, True, False, False] -- [True,True,False,False]
-- main = print s12

s13 = scanr1 (+) [1 ..] -- error * Hangs forever * stack overflow
-- main = print s13

-- iterate
-- iterate f x returns an infinite list of repeated applications of f to x
-- iterate f x == [x, f x, f (f x), f (f (f x))), ...]

i1 = take 10 (iterate (+ 3) 42)

-- main = print i1 -- [42,45,48,51,54,57,60,63,66,69]

i2 = take 10 (iterate (* 2) 10)

-- main = print i2 -- [10,20,40,80,160,320,640,1280,2560,5120]

i3 = take 5 (iterate (^ 2) 10)

-- main = print i3 -- [10,100,10000,100000000,10000000000000000]

-- repeat
-- repeat :: a -> [a]
-- repeat x is an infinite list, with x the value of every element

r1 = take 6 $ repeat 11 -- take 6 (repeat 11)
-- main = print r1 -- [11,11,11,11,11,11]

r2 = take 5 (repeat [1])

-- main = print r2 -- [[1],[1],[1],[1],[1]]

-- replicate
-- replicate :: Int -> a -> [a]
-- replicate n x is a list of length n with x the value of every element

r3 = replicate 0 True -- []
-- main = print r3

r4 = replicate (-1) True -- []
-- main = print r4

r5 = replicate 3 False -- [False,False,False]
-- main = print r5

-- contniue from here!!

-- cycle :: [a] -> [a]
-- cycle ties a finite list into a circular one, or equivalently
-- is the infinite repetition of the original list
-- It is the identity on infinite lists.
-- cycle :: [a] -> [a]
-- cycle [] = error "cycle"
-- cycle xs = xs' where xs' = xs ++ xs'

c1 = take 10 (cycle "ABC") -- "ABCABCABCA"
-- main = print c1

c2 = take 2 (cycle (42 : 42 : undefined)) -- [42,42]  -- no ned to cycle in such cases
-- main = print c2

-- c3 = take 3 (cycle [1..5]) -- [1,2,3,4,5,1,2,3,4,5,1,2,3]
-- main = print c3

-- Tuples
fst2 :: (a, b) -> a
fst2 (a, b) = a

-- returns the first item in a tuple

-- main = print (fst2("xyz",([1..5],45,'q'))) -- "xyz"
-- main = print (take 2 (cycle [1, 2, 3]))

-- snd ::	(a,b) -> b
-- returns the second item in a tuple

-- main = print (snd ("xyz",([1..5],45,'q'))) -- ([1,2,3,4,5],45,'q')

-- Zipping
-- zip - takes two lists and returns a list of corresponding pairs.
-- zip :: [a] -> [b] -> [(a, b)]
-- main = print (zip [1..5] ['a'..'h'])
-- zip logic:
zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] [] = []
zip1 xs [] = []
zip1 [] ys = []
zip1 (x : xs) (y : ys) = (x, y) : zip1 xs ys

-- main = print (zip1 ["alma", "barack", "csoki"] ['a'..'h'])
-- [("alma",'a'),("barack",'b'),("csoki",'c')]

-- main = print (zip "ABCDE" [1,2,3]) -- [('A',1),('B',2),('C',3)]

-- main = print (zip "AB" [1,2,3,4,5]) -- [('A',1),('B',2)]

-- main = print (zip "ABC" [1]) -- [('A',1)]

-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
-- zip3 takes three lists and returns a list of triples, analogous to zip
-- first from the first list, second from second list and third from third list

-- main = print (zip3 [1..5]  ['o'..'z'] [100..])
-- [(1,'o',100),(2,'p',101),(3,'q',102),(4,'r',103),(5,'s',104)]

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith (,) xs ys == zip xs ys
-- zipWith f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]

-- main = print (zipWith (,) [1..5] [1..5]) -- [(1,1),(2,2),(3,3),(4,4),(5,5)] it is just zip

-- main = print (zipWith (*) [1..5] [1..5]) -- [1,4,9,16,25]

-- main = print (zipWith (**) (replicate 10 3) [1..10])
-- [3.0,9.0,27.0,81.0,243.0,729.0,2187.0,6561.0,19683.0,59049.0]

-- main = print (zipWith (++) [".", "."] ["hello", "world!"]) -- [".hello",".world!"]

-- main = print (zipWith (\x y -> 2*x + y) [1..4] [5..8])
-- [2*1+5,2*2+6,2*3+7,2*4+8] = [7,10,13,16]

-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- the zipWith3 function takes a function which combines three elements,
-- as well as three lists and returns a list of the function applied
-- to corresponding elements, analogous to zipWith.
-- zipWith3 (,,) xs ys zs == zip3 xs ys zs
-- zipWith3 f [x1,x2,x3..] [y1,y2,y3..] [z1,z2,z3..] == [f x1 y1 z1, f x2 y2 z2, f x3 y3 z3..]

-- main = print (zipWith3 (,,) [1..5] [1..5] [1..5])
-- [(1,1,1),(2,2,2),(3,3,3),(4,4,4),(5,5,5)] it is just zip3

-- main = print (zipWith3 (\x y z -> x*y*z) [1..3] [10..] [20..])
-- [1*10*20, 2*11*21, 3*12*22] = [200,462,792]

-- main = print (zipWith3 (\x y z -> [x]++[y]++[z]) [1, 2, 3] [4, 5, 6] [7, 8, 9])
-- [[1]++[4]++[7],[2]++[5]++[8],[3]++[6]++[9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

-- end review of previous lectures

-- Unzipping

-- unzip :: [(a, b)] -> ([a], [b])
-- unzip transforms a list of pairs into a list of
-- first components and a list of second components

-- main = print (unzip [(1, 'a'), (2, 'b'), (3, 'c'), (4,'d')])
-- ([1,2,3,4],"abcd")

-- main = print (unzip [(1, "abc"), (2, "def"), (3, "ghi"), (4,"jkl")])
-- ([1,2,3,4],["abc","def","ghi","jkl"])

-- main = print (unzip [])
-- error ambigous ([],[])

-- unzip3 :: [(a, b, c)] -> ([a], [b], [c])
-- unzip3 function takes a list of triples
-- and returns three lists of the respective components, analogous to unzip

-- main = print (unzip3 [(1, 'a', True), (2, 'b', False), (3, 'c', True)])
-- ([1,2,3],"abc",[True,False,True])

-- main = print (unzip3 [(1, True, 2.0), (2, False, 4.0), (3, True, 6.0)])
-- ([1,2,3],[True,False,True],[2.0,4.0,6.0])

-- Special folds

-- and :: [Bool] -> Bool
-- returns the conjunction of a list of Bools

-- main = print (and [True, True, False]) -- False

-- main = print (and (False : repeat True)) -- Infinite list [False,True,True,True,... ] -- False

-- main = print ( and (repeat True)) -- error * Hangs forever *

-- or :: [Bool] -> Bool
-- returns the disjunction of a list of Bools

-- main = print (or [True, True, False]) -- True

-- main = print (or (True : repeat False)) -- Infinite list [True,FalseFalse,False,...] -- True

-- main = print (or (repeat False)) -- error * Hangs forever *

-- any :: (a -> Bool) -> [a] -> Bool
-- determines whether any element of the structure satisfies the predicate

-- main = print (any (> 3) []) -- False

-- main = print (any (> 3) [1,2]) -- False

-- main = print (any (> 3) [1,2,3,4,5]) -- True

-- main = print (any (> 3) [1..]) -- True

-- main = print (any (> 3) [0, -1..]) -- error * Hangs forever *

-- all :: (a -> Bool) -> [a] -> Bool
-- determines whether all elements of the structure satisfy the predicate

-- main = print (all (> 3) []) -- True

-- main = print (all (> 3) [1,2]) -- False

-- main = print (all (> 3) [1,2,3,4,5]) -- False

-- main = print (all (> 3) [1..]) -- False

-- main = print (all (> 3) [4..]) -- error * Hangs forever *

-- concat :: [[a]] -> [a]
-- The concatenation of all the elements of a container of lists.

-- main = print (concat [[1, 2, 3], [4, 5], [6], []]) -- [1,2,3,4,5,6]

-- concatMap :: (a -> [b]) -> [a] -> [b]
-- Maps a function over all the elements of a list
-- and concatenates the resulting lists

-- main = print (concatMap (take 3) [[1..], [10..], [100..], [1000..]])
-- [1,2,3]++[10,11,12]++[100,101,102]++[1000,1001,1002]
-- [1,2,3,10,11,12,100,101,102,1000,1001,1002]

-- main = print (concatMap (take 3) [[1..], [1..], [1..]])
-- [1,2,3] ++ [1,2,3] ++ [1,2,3]
-- [1,2,3,1,2,3,1,2,3]

-- main = print (concatMap tail [["abc", "def", "ghc", "haskell"], ["abcdef",""], ["hello", "friends", "today"]])
-- ["def", "ghc", "haskell"] ++ [""] ++ ["friends", "today"]]
-- ["def","ghc","haskell","","friends","today"]

-- main = print (concatMap (map (\ x -> x + 3)) [[1..5], [5..10], [11..20]])
-- [map (\ x -> x + 3) [1..5]] ++ [map (\ x -> x + 3) [5..10]] ++ [map (\ x -> x + 3) [11..15]]
-- [4,5,6,7,8] ++ [8,9,10,11,12,13] ++ [14,15,16,17,18,19,20,21,22,23]
-- [4,5,6,7,8,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

-- span :: (a -> Bool) -> [a] -> ([a], [a])
-- applied to a predicate p and a list xs,
-- returns a tuple where first element is the longest prefix (possibly empty) of xs
-- of elements that satisfy p and second element is the remainder of the list:
-- span p xs is equivalent to (takeWhile p xs, dropWhile p xs)

sc1 = span (< 3) [1, 2, 3, 4, 1, 2, 3, 4] -- ([1,2],[3,4,1,2,3,4])
-- main = print sc1

sc2 :: ([Integer], [Integer])
sc2 = span (< 9) [1, 2, 3] -- ([1,2,3],[])
-- main = print sc2

sc3 = span (< 0) [1, 2, 3] -- ([],[1,2,3])
-- main =s print sc3

sc4 = span (const True) (1 : [2 .. 5]) -- ([1,2,3,4,5],[])
-- main = print sc4

-- sc5 = take 1 (fst (span (const True) (1 : undefined))) -- [1]
-- main = print sc5

-- sc6 = take 10 (fst (span (const True) [1..])) -- [1,2,3,4,5,6,7,8,9,10]
-- main = print sc6

-- break
-- break :: (a -> Bool) -> [a] -> ([a], [a])
-- applied to a predicate p and a list xs,
-- returns a tuple where first element is longest prefix (possibly empty) of xs
-- of elements that do not satisfy p and second element is the remainder of the list
-- break p is equivalent to span (not . p)
-- consequently t1o (takeWhile (not . p) xs, dropWhile (not . p) xs)

b1 = break (> 3) [1, 2, 3, 4, 1, 2, 3, 4] -- ([1,2,3],[4,1,2,3,4])
-- main = print b1

b2 = break (< 9) [1, 2, 3] -- ([],[1,2,3])
-- main = print b2

b3 = break (> 9) [1, 2, 3] -- ([1,2,3],[])
-- main = print b3

-- splitAt :: Int -> [a] -> ([a], [a])
-- splitAt n xs returns a tuple where first element is
-- xs prefix of length n and second element is the remainder of the list
-- splitAt n xs is equivalent to (take n xs, drop n xs)

p1 = splitAt 6 "Hello World!" -- ("Hello ","World!")
-- main = print p1

p2 = splitAt 3 [1, 2, 3, 4, 5] -- ([1,2,3],[4,5])
-- main = print p2

p3 = splitAt 1 [1, 2, 3] -- ([1],[2,3])
-- main = print p3

p4 = splitAt 3 [1, 2, 3] -- ([1,2,3],[])
-- main = print p4

p5 = splitAt 4 [1, 2, 3] -- ([1,2,3],[])
-- main = print p5

p6 = splitAt 0 [1, 2, 3] -- ([],[1,2,3])
-- main = print p6

p7 = splitAt (-1) [1, 2, 3] -- ([],[1,2,3])
-- main = print p7

-- lookup
-- looks up a key in a list of tuples
-- returns the pair of the key
-- For the result to be Nothing, the list must be finite.
-- lookup :: a -> [(a, b)] -> b

l1 = lookup 2 [] -- error
-- main = print l1

l2 :: Maybe String
l2 = lookup 2 [(1, "first")] -- Nothing
-- main = print l2

l3 :: Maybe String
l3 = lookup 3 [(1, "first"), (2, "second"), (3, "third")] -- Just "second"
-- main = print l3

------------------

-- Generators - list comprehension

s :: String
s = "Hello"

g1 :: [Char]
g1 = [toUpper c | c <- s]

-- main = print g1 -- "HELLO"

-- Strings in Haskell are lists of characters;
-- the generator c <- s feeds each character of s
-- to the left-hand expression toUpper c, building a new list.
-- The result of this list comprehension is "HELLO".
-- in this simple example you can just write map toUpper s.

-- double all elements

g2 = [2 * x | x <- [1 .. 10]]

-- main = print g2 -- [2,4,6,8,10,12,14,16,18,20]

g3 = [(3 * x, x * x, x ^ 3) | x <- [1 .. 5]]

-- main = print g3
-- [(3,1,1),(6,4,8),(9,9,27),(12,16,64),(15,25,125)]

-- multiple generators, separated by commas
-- Cartesian product (Descartes product) from sets A x B

g4 = [(i, j) | i <- [1, 2], j <- [1 .. 4]]

-- main = print g4
-- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]

-- each successive generator refines the results of the previous generator
-- if the second list is infinite, one will never reach
-- the second element of the first list

g5 = take 10 [(i, j) | i <- [1, 2], j <- [1 ..]]

-- main = print g5
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]

-- in such a situation, a nested sequence of list comprehensions may be appropriate

g6 = take 5 [[(i, j) | i <- [1, 2]] | j <- [1 ..]]

-- main = print g6
-- [[(1,1),(2,1)], [(1,2),(2,2)], [(1,3),(2,3)], [(1,4),(2,4)], [(1,5),(2,5)]]

-- we can also provide boolean guards

g7 = [(x, y) | x <- [1 .. 4], y <- [1 .. 5], y <= x]

-- main = print g7
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)]

g8 = [(i, j) | i <- [1 .. 5], j <- [1 .. i - 1], even j]

-- main = print g8
-- [(3,2),(4,2),(5,2),(5,4)]

g9 = take 10 [(i, j) | i <- [1 ..], j <- [1 .. i - 1], gcd i j == 1]

-- main = print g9
-- [(2,1),(3,1),(3,2),(4,1),(4,3),(5,1),(5,2),(5,3),(5,4),(6,1)]

g10 = [(w, x) | w <- [1 .. 4], x <- [1 .. 4], w /= x]

-- main = print g10
-- [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]