
main :: IO ()
-- -> lab 2 tasks
-- -----------------
-- 1. Given a positive integer, find the sum of the odd numbers up to that number starting from 1.
-- ex:  5 -> 5+3+1
sumOdd :: Int ->  Int
sumOdd 1 = 1
sumOdd x
 |x`mod`2 == 0 = sumOdd(x-1)
 |otherwise=x+sumOdd(x-1)


-- main = do
--     print(sumOdd 5 )
--     print(sumOdd 1 )

-- 2. Compute the sum of the squares of numbers from 1 to n.
-- 3 -> 1^2 + 2^2 + 3^3 
sumOfSquares :: Int -> Int
sumOfSquares 1= 1
sumOfSquares n=n^2 + sumOfSquares(n-1)
 
-- main = do
--    print(sumOfSquares 3);  -- 9 + 4 + 1 =14
--    print(sumOfSquares 2); -- 4+1 =5
--    print(sumOfSquares 5); -- 25+16+9+4+1 = 51
--    print(sumOfSquares 1); -- 1 = 1
--    print(sumOfSquares 0); -- 0 = stackoverflow

-- 3. Write a function multiplyUntilOne that takes an integer n and returns the result of multiplying
--    n by itself n times, then decreasing n by 1 after each multiplication until n reaches 1.

multiplyUntilOne :: Int -> Int
-- 3 -> 3*3 
multiplyUntilOne 1= 1
multiplyUntilOne n = n^n + multiplyUntilOne(n-1)
-- main = print(multiplyUntilOne 3)
-- 4. Write a function which calculates the sum of the digits of a number.
-- 123 -> 1+2+3  = 6
digitSum :: Int -> Int
digitSum 0 = 0      
digitSum n = n`mod`10 + digitSum(n `div` 10)

-- main = do
--     print(digitSum 123) -- 6
--     print(digitSum 333) -- 9
--     print(digitSum 235) ---10

-- 5. Write a function that takes two arguments, say n and x, and computes their power,
-- x ==3, n = 2 so 3^2 =9
powerrec ::  Int -> Int -> Int
powerrec x n
     |n==0 =1
     |otherwise=x*powerrec x (n-1)

-- main = do 
--     print(powerrec 2 0);
--     print(powerrec 3 3);      
-- 6. Returns the number decreased by the last digit if positive, otherwise returns -1.
-- 7.  Return the product if both numbers are odd, sum if both are even, otherwise return 0.
-- 8. Check if 5 numbers are sorted in increasing order.
-- 9. Transform days into years, weeks, and days.
-- Convert the number of days into a string of years, weeks, and days    