{-# LANGUAGE ParallelListComp #-}
main :: IO ()



-- Records

-- Triple tuple (,,) is the constructor of triples
type PersonTup = (String, (Int,Int,Int), Bool)

xy :: PersonTup
xy = ("XY", (2000,1,1), True)

--main = print xy
--("XY",(2000,1,1),True)


-- record = named tuple, the components are called fields
-- record composed data structure with components of different types
-- MkDate is called the constructor of the record Date

data Date = MkDate { year :: Int, month :: Int, day :: Int } deriving (Show)

-- Date is type name
-- MkDate is type constructor name
-- the deriving (Show) ensures the data type can be displayed using show function

today, day1, day2 :: Date

-- positional initialization
today = MkDate 2024 11 21
--main = print today
--MkDate {year = 2024, month = 11, day = 21}

day1 = MkDate 20 11 2024 
--main = print day1 -- compiles but wrong order!
--MkDate {year = 20, month = 11, day = 2024}

-- with names the order does not matter
day2 = MkDate { year = 2024, day = 20, month = 11} 
--main = print day2
--MkDate {year = 2024, month = 11, day = 20}

--day3 :: Date
--day3 = MkDate { year = 2024 }
--main = print day3
--warning: [-Wmissing-fields] * Fields of `MkDate' not initialised:
--        month :: Int
--        day :: Int


-- nested records
-- Person record with three fields, one of them is the record Date
data Person = MkPerson { name       :: String
                       , birthdate  :: Date
                       , programmer :: Bool
                       }
                       deriving (Show)

-- Person type name
-- MkPerson constructor name
-- type and constructor name can be the same name
-- 3 fields of 3 different types: name, birthdate, programmer

p :: Person = MkPerson
  { name = "Peter"
  , birthdate = today
  , programmer = True }
  
-- how to extract field values?
-- field names become projections (getter functions)

getName, getName1, getName2, getName3, getName4 :: Person -> String
getName = name
--main = print $ getName p -- "Peter"

getName1 (MkPerson { birthdate = _, name = s, programmer = _ } ) = s
--main = print $ getName1 p -- "Peter"

getName2 (MkPerson { name = s } ) = s 
--s is local variable equal to name
--main = print $ getName2 p -- "Peter"

getName3 (MkPerson { name } ) = name
--main = print $ getName3 p -- "Peter"

getName4 (MkPerson nm bd prog) = nm
--main = print $ getName4 p -- "Peter"


-- updating records
changeName, changeName1 :: Person -> String -> Person
newName = "Mary"

-- takes person per and changes name to n
changeName per n = per {name = n}
--main = print $ changeName p newName
--MkPerson {name = "Mary", birthdate = MkDate {year = 2024, month = 11, day = 21}, programmer = True}

name2 = "John"
-- expects the input in a record
-- creates new record with the name filed n and bd and pr taken from input record 
changeName1 (MkPerson nn bd pr) n = MkPerson n bd pr
--main = print $ changeName1 p name2
--MkPerson {name = "John", birthdate = MkDate {year = 2024, month = 11, day = 21}, programmer = True}

--the name of the original person p is not changed, i.e. immutable
--main = print $ getName p -- "Peter"


obtainedPhD, obtainedPhD2 :: Person -> Person
obtainedPhD (MkPerson n bd pr) = MkPerson ("dr. " ++ n) bd pr
obtainedPhD2 per = per { name = "dr. " ++ name per }
--main = print $ getName (obtainedPhD2 p) --"dr. Peter"
--main = print $ getName p -- "Peter"

obtainedDegree, obtainedDegree2 :: Person -> Person
obtainedDegree (MkPerson n bd pr) = MkPerson n bd True
obtainedDegree2 per = per { programmer = True }
--main = print $ obtainedDegree p
--MkPerson {name = "Peter", birthdate = MkDate {year = 2024, month = 11, day = 21}, programmer = True}


-- sorting a list of persons and selecting the oldest
lt :: Person -> Person -> Bool
lt (MkPerson { birthdate = MkDate { year = y1, month = m1, day = d1 } })
   (MkPerson { birthdate = MkDate { year = y2, month = m2, day = d2 } })
  = (y1 < y2) || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 < d2)

lte :: Person -> Person -> Bool
lte (MkPerson { birthdate = MkDate { year = y1, month = m1, day = d1 } })
    (MkPerson { birthdate = MkDate { year = y2, month = m2, day = d2 } })
  = (y1 < y2) || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 <= d2)

psort :: [Person] -> [Person]
psort  []    = []
psort (x:xs) = psort [y | y <- xs, y `lt` x ] ++ [x] ++ psort [y | y <- xs, x `lte` y]

oldest :: [Person] -> Person
oldest ps = head $ psort ps

oldestProgrammer :: [Person] -> Person
oldestProgrammer ps = oldest (filter programmer ps)

p1, p2, p3, p4 :: Person
p1 = MkPerson { name = "B", birthdate = MkDate 2000 1 1, programmer = False }
p2 = MkPerson { name = "D", birthdate = MkDate 3000 1 1, programmer = True }
p3 = MkPerson { name = "C", birthdate = MkDate 2000 2 2, programmer = True }
p4 = MkPerson { name = "A", birthdate = MkDate 2000 1 1, programmer = True }
ppl :: [Person]
ppl = [p1,p2,p3,p4]

--main = print $ oldest ppl
--MkPerson {name = "B", birthdate = MkDate {year = 2000, month = 1, day = 1}, programmer = False}

-- Exmple: points
data Point = MkPoint { x :: Float, y :: Float, visible :: Bool } deriving (Show)

origo :: Point
origo = MkPoint { x = 0 , y = 0 , visible = True }
--main = print $ origo
--MkPoint {x = 0.0, y = 0.0, visible = True}

data Vector = MkVector { dx :: Float , dy :: Float } deriving (Show)
distance :: Vector
distance = MkVector { dx = 2.0, dy = 3.0 }

hide :: Point -> Point
hide p = p { visible = False }

move :: Point -> Vector -> Point
move p v = p { x = x p + dx v , y = y p + dy v }

--main = print $ move (hide origo) distance
--MkPoint {x = 2.0, y = 3.0, visible = False}

-- Example: rational numbers with numerator, denominator integer fields
data Q = Q { nom :: Int , den :: Int } deriving (Show)
-- type name and constructor name can be the same name

-- 1/3, third, numerator = 1, denominator = 3
half, third :: Q
half = Q { nom = 1, den = 2 }
third = Q { den = 3 , nom = 1 }
--main = print $ (half, third)
--(Q {nom = 1, den = 2},Q {nom = 1, den = 3})

simplify :: Q -> Q
simplify Q { nom=n, den=d }
   | d == 0 = error " denominator is 0"
   | d < 0 = Q { nom = (-n) `div` g, den = (-d) `div` g}
   | otherwise = Q { nom = n `div` g, den = d `div` g}
      where g = gcd n d

-- Euclidean algorithm of gcd - only works for positive numbers
gcdE :: Int -> Int -> Int
gcdE x y | x >  y = gcdE (x - y) y
         | x <  y = gcdE (y - x) x
         | x == y = x


mkQ :: Int -> Int -> Q
mkQ n d = simplify (Q n d)
--main = print $ mkQ 81 999
--Q {nom = 3, den = 37}
--Q operations at practices !!



-- Algebraic Data Types

-- Enumeration Types
-- Like many programming languages, Haskell allows programmers to 
-- create own enumeration types

data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)

-- this declares a new type called Days with 7 data values 
-- Mon, Tue, etc which are the (only) values of type Days
-- deriving Show tells GHC to automatically generate default code 
-- for converting Days to Strings to be able to print

tomorrow :: Days
tomorrow = Fri

--main = print $ tomorrow -- Fri

listlistofDays :: [Days]
listlistofDays = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

