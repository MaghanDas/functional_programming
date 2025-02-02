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
