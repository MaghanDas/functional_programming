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


-- write functions on Days by pattern matching
isWorkday :: Days -> Bool
isWorkday Sat = False
isWorkday Sun = False
isWorkday _ = True

--main = print $ isWorkday tomorrow -- True

-- nextDay function by cases
nextDay :: Days -> Days
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

--main = print $  nextDay Thu -- Fri

-- nextDay function by indices, needs == on Days, so Eq is included in deriving list
nextDayI :: Days -> Days
nextDayI d = listlistofDays !! (n+1)
   where n = head [i | (u,i) <- zip listlistofDays [0..], u == d]

--main = print $ nextDayI Thu -- Fri


-- Type constructors

data CheckedInt = Failure 
                | OK Int
                deriving Show

-- OK constructor takes an argument of type Int 
-- OK by itself is not a value of type CheckedInt
-- we need to give to it an Int 
-- For example, OK 10 is a value of type CheckedInt
-- for any x of type Int, OK x has type CheckedInt
-- OK :: Int -> CheckedInt
-- Failure constructor is just one value: Failure

-- show
a = Failure
b = OK 10

--main = print (a,b) -- (Failure,OK 10)

-- how to use
safeDiv :: Int -> Int -> CheckedInt
safeDiv _ 0 = Failure
safeDiv a b = OK (a `div` b)

--main = print (safeDiv 2 0, safeDiv 10 3 ) -- (Failure,OK 3)

-- pattern matching for constructors
transformToZero :: CheckedInt -> Int
transformToZero Failure = 0
transformToZero (OK f) = f

--main = print (transformToZero Failure, transformToZero (OK 7)) -- (0,7)

-- constructors can have more than one argument
-- store a worker name, age, and working day under one construct

data Worker = Worker String Int Days
  deriving Show

bela :: Worker
bela = Worker "Bela" 30 Fri

jozsi :: Worker
jozsi = Worker "Jozsi" 40 Mon

getWday :: Worker -> Days
getWday (Worker _ _ a) = a

--main = print (getWday bela) -- Fri

-- patterns can be nested into each other: in Worker type we used Days type
-- show function can bes used if deriving Show is given
-- x@pattern gives the name x to the entire value being matched
-- here we use putStrLn, because the value we are displaying is already a String

workerString :: Worker -> String
workerString p@(Worker n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

--main = putStrLn (workerString bela)
-- The name field of (Worker "Bela" 30 Fri) is Bela
--main = print $ (workerString bela)
-- "The name field of (Worker \"Bela\" 30 Fri) is Bela"



-- Algebraic Data Types in general

-- In general, an algebraic data type has one or more data constructors 
-- and each data constructor can have zero or more arguments

{- 
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
-}

-- a value of type AlgDataType can be constructed in one of 4 ways: 
-- using Constr1, Constr2, Constr3, or Constr4
-- Depending on the constructor used, an AlgDataType value may contain some other values 
-- Constr1 comes along with two values, one of type Type11 and one of type Type12 and so on

-- types and data constructor names must always start with a capital letter
-- variables (including names of functions) must always start with a lowercase letter

-- pattern matching is about taking apart a value by finding out which constructor it was built with
-- to decide what to do with a value of type AlgDataType needs pattern matching

{- 
foo (Constr1 a b)   = ...
foo (Constr2 a)     = ...
foo (Constr3 a b c) = ...
foo Constr4         = ...
-}

-- by this we also give names to the values that come along with each constructor
-- a b c give names to the values being matched
-- parentheses are required around patterns consisting of more than just a single constructor
-- underscore _ can be used as a “wildcard pattern” which matches anything

{-
case exp of
  pattern1 -> expression1
  pattern2 -> expression2
  ...
-}

-- case can be used for multiple patterns
transformToZero2 :: CheckedInt -> Int
transformToZero2 p = case p of
  Failure -> 0
  OK d    -> d
--main = print (transformToZero2 Failure, transformToZero2 (OK 10)) -- (0,10)



-- Recursive Algebraic Data Types

-- Data types can be recursive, i.e. defined in terms of themselves
-- we have already seen a recursive type: the type of lists 
-- A list is either empty, or a single element followed by a remaining list.
-- We can define our own list type like:

data IntList = Empty | Cons Int IntList
intListSum :: IntList -> Int
intListSum Empty = 0
intListSum (Cons x xs) = x + intListSum xs

--main = print (intListSum (Cons 4 (Cons 3 (Cons 2 Empty)))) -- 9
-- it is similar to 4:3:2:[]



-- Double recursive data types: Trees

-- we can define a type of binary trees with 
-- an `a` type value stored at each internal node

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

-- 2 constructors: Node and Leaf
-- arity is the number of arguments
-- Leaf has arity 0 (no arguments) and 
-- Node has arity 3: a value and two sub-trees

tree1:: Tree Int
tree1 = (Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf)
 
--main = print tree1
-- Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf

{-
          4                  +1 level
        /   \
       2    Leaf             +1 level 
     /  \    
    1    3                   +1 level   
   / \   / \
LeafLeaf Leaf Leaf           +0 level
                             depth is 3
-}

-- finding the number of nodes
size :: Tree a -> Int
size Leaf = 0
size (Node _ left right) = 1 + size left + size right

--main = print $ size tree1 -- 4
-- 1 + size left + size right
-- 1 + 1 + size left' + size right'
-- 1 + 1 +  1 + 0 + 0 +   1 + 0 + 0
-- 4 

-- traversing and collecting values of nodes inorder: left - root - right 
collect :: Tree Int -> [Int]
collect Leaf = []
collect (Node x left right) = collect left ++ [x] ++ collect right

--main = print $ collect tree1
{- collect 2  ++ [4] ++ collect Leaf 
         /  \    
        1    3
      / \   / \
  LeafLeaf Leaf Leaf 

  collect 1 ++ [2] ++ collect 3 ++ [4] ++ []
         / \                 / \
      Leaf  Leaf            Leaf Leaf 

  collect Leaf ++ [1] ++ collect Leaf ++ [2] ++ collect Leaf ++ [3] ++ collect Leaf ++ [4] ++ []
[] ++ [1] ++ [] ++ [2] ++ [] ++ [3] ++ [] ++ [4] ++ []
[1,2,3,4]
-}

--collecting preorder: root - left - right
collectPre :: Tree Int -> [Int]
collectPre Leaf = []
collectPre (Node x left right) = [x] ++ collectPre left ++ collectPre right

--main = print $ collectPre tree1
-- [4,2,1,3]

--collecting postorder: left - right - root
collectPost :: Tree Int -> [Int]
collectPost Leaf = []
collectPost (Node x left right) = collectPost left ++ collectPost right ++ [x]

--main = print $ collectPost tree1
-- [1,3,2,4]

-- number of layers (levels) where there are nodes
depth:: Tree a -> Int
depth Leaf = 0
depth (Node _ left right) = (max (depth left) (depth right)) + 1 

--main = print $ depth tree1 -- 3