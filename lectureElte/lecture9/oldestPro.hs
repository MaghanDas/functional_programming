import Data.List (sortBy)

data Date = MkDate { year :: Int, month :: Int, day :: Int } deriving (Show, Eq)
data Person = MkPerson { name :: String, birthdate :: Date, programmer :: Bool } deriving (Show, Eq)

-- Comparator for sorting by birthdate
compareByBirthdate :: Person -> Person -> Ordering
compareByBirthdate (MkPerson { birthdate = MkDate y1 m1 d1 })
                                  (MkPerson { birthdate = MkDate y2 m2 d2 })
    | y1 < y2 = LT
    | y1 > y2 = GT
    | m1 < m2 = LT
    | m1 > m2 = GT
    | d1 < d2 = LT
    | d1 > d2 = GT
    | otherwise = EQ

-- Sort persons by birthdate
psort :: [Person] -> [Person]
psort = sortBy compareByBirthdate

-- Find the oldest person
oldest :: [Person] -> Person
oldest = head . psort

-- Find the oldest programmer
oldestProgrammer :: [Person] -> Person
oldestProgrammer = oldest . filter programmer

-- Sample Data
p1, p2, p3, p4 :: Person
p1 = MkPerson { name = "B", birthdate = MkDate 2000 1 1, programmer = False }
p2 = MkPerson { name = "D", birthdate = MkDate 3000 1 1, programmer = True }
p3 = MkPerson { name = "C", birthdate = MkDate 2000 2 2, programmer = True }
p4 = MkPerson { name = "A", birthdate = MkDate 2000 1 1, programmer = True }

ppl :: [Person]
ppl = [p1, p2, p3, p4]

-- Main Function
main :: IO ()
main = do
    print $ oldest ppl
    print $ oldestProgrammer ppl
