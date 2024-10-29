
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
getLastPositive :: Int -> Int
getLastPositive n
       |n < 0 = -1
       |n <10 = 0
       |otherwise = n - (n `mod` 10)
-- main = do
--      print(getLastPositive 23)
--      print(getLastPositive 344

-- 7.  Return the product if both numbers are odd, sum if both are even, otherwise return 0.
-- Function to return the product if both numbers are odd, sum if both are even, otherwise return 0
processNumbers :: Int -> Int -> Int
processNumbers x y
  | odd x && odd y   = x * y            -- Both numbers are odd, return the product
  | even x && even y = x + y            -- Both numbers are even, return the sum
  | otherwise        = 0                -- One number is odd, the other is even, return 0
-- main = do
  print (processNumbers 3 5)    -- Both odd, should return 15 (3 * 5)
  print (processNumbers 4 8)    -- Both even, should return 12 (4 + 8)
  print (processNumbers 3 4)    -- One odd, one even, should return 0
  print (processNumbers 7 2)    -- One odd, one even, should return 0

-- 8. Check if 5 numbers are sorted in increasing order.
    isSorted :: [Int] -> Bool
isSorted [] = True -- empty list is sorted always
isSorted [_] = True -- single elements is always sorted.
isSorted( x:y:xs ) = (x <=y) && isSorted(y:xs)
-- main = do
--              print (isSorted[1,2,3,4,5])
--              print (isSorted[3,2,1])


-- 9. Transform days into years, weeks, and days.
aysToYearsWeeksDays :: Int -> String
daysToYearsWeeksDays days = 
  let years = days `div` 365          -- Calculate the number of years
      weeks = (days `mod` 365) `div` 7 -- Calculate the number of weeks
      remainingDays = (days `mod` 365) `mod` 7 -- Calculate remaining days
  in  show years ++ " years, " ++ show weeks ++ " weeks, and " ++ show remainingDays ++ " days"
-- Example usage:
-- main :: IO ()
-- main = putStrLn (daysToYearsWeeksDays 800) -- "2 years, 10 weeks, and 1 days"


-- 10. ---- Armstrong number
--  If sum of cubes of each digit of the number is equal to the number itself, then the number is called an   Armstrong number.
--  153 = 1^3 + 5^3 + 3^3
--  Given a positive integer number, write a function to determine whether it is an Armstrong number or not.

-- Function to check if a number is an Armstrong number
isArmstrong :: Int -> Bool
isArmstrong n = n == sumOfCubes n
  where
    -- Helper function to calculate the sum of cubes of digits
    sumOfCubes :: Int -> Int
    sumOfCubes 0 = 0
    sumOfCubes x = (x `mod` 10) ^ 3 + sumOfCubes (x `div` 10)
-- main = print( isArmstrong 153) -- True
-- main = print( armstrong 370) -- True
-- main = print( armstrong 0) -- True
-- main = print( armstrong 12) -- False