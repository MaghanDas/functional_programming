-- Define some values and a function
myName = "Haskell Learner"
favoriteNumber = 7
isHaskellFun = True
-- greet name = "Hello, " ++ name ++ "!"

describeAge :: Int -> String
describeAge age = let ageGroup = if age < 18 then "young" else "mature"
                               in "you are " ++ ageGroup
-- Main function to print these values
main = do
        -- putStrLn(describeAge 3)
        print (describeAge 5)
    -- print isHaskellFun
--   putStrLn ("My name is " ++ myName)
--   putStrLn ("My favorite number is " ++ show favoriteNumber)
--   putStrLn ("Is Haskell fun? " ++ show isHaskellFun)
--   putStrLn (greet "john")