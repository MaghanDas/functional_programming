-- .hs haskell file

someNumber = 12  -- end of line comment

{-
mult-line comment
-}

-- ghci: Glasgow Haskell Compiler Interpreter
-- runghc pres02.hs

{- Basic types and functions

program :: Type

-- Int is -2^63 ... +2^63  --- altogether 64 bits, 2^64 different numbers

1 :: Int
123231321324980349810498 :: Integer -- try with Int!
1/3 :: Rational -- precise rationals
5.412 :: Double -- floating point number
1.1-1 :: Double


+ * / ^ -
abs, gcd  -- try: abs -1
-- difference between negation and subtraction: -1 vs. 2-1
-- correct way to write abs -1 is abs (-1)
sqrt, sin, exp
Bool: <, <=, ==, /=, &&, ||, if-then-else

&& and
a && b = (are both a and b True?)
True  && True  = True
True  && False = False
False && True  = False
False && False = False

|| or
a || b = (is at least one of a or b True?)
False || False = False
False || True  = True
True  || False = True
True  || True  = True

if a then b else c = ???
if True  then b else c = b
if False then b else c = c

if 1 == 1 then b else c = ???
  if 1 == 1 then b else c = 
  if True then b else c = 
  b

f a b = a+b*a  -- prefix / ordinary function
-}

ifthenelse True  a b = a
ifthenelse False a b = b

{-
Char
'a' :: Char

String, ++, !!
++ concatenation
"hello world" :: String

() :: ()
-}

-- Plan9, Haiku OS (BeOS), Oberon
-- Alt-F2  cmd ENTER

-- IO () means that this function does input-output and returns something of type ()
-- () is the unit type which only has one element which is also called ()
-- main :: IO ()
-- main = putStrLn "Hello World!"

-- compile a program: ghc pres02.hs
-- run a compiled program:
--   Windows: pres02.exe (or double click on the file)
--   Linux or Mac: ./pres02

-- compile and immedietaly run the program: 
--   runghc pres02.hs

-- load the program in ghci:
--   ghci pres02.hs
-- same as
--   ghci
--   :l pres02.hs


{- Function application

In mathematics, function application is denoted using parentheses, and
multiplication is often denoted using juxtaposition or space.

f(a,b) + c d

^ Apply the function f to a and b, and add the result to the product
  of c and d.

f a b + c*d

f a + b

^ means (f a) + b, not f (a + b)

   +
  / \
 f   b         (f a) + b
 |
 a 

   f
   |
   +           f (a + b)
  / \
 a   b


   +
  / \
 1   *        1+2*3 means 1+(2*3)
    / \
   2   3

   *
  / \
 +   3        (1+2)*3
/ \
1  2

| math      | Haskell   |
|-----------+-----------|
| f(x)      | f x       |
| f(x,y)    | f x y     |
| f(g(x))   | f (g x)   |
| f(x,g(y)) | f x (g y) |
| f(x)g(y)  | f x * g y |
-}


absolute0 x = if (x<0) then (-x) else x


-- main = print (absolute0 (-10))
{-
absolute0 (-10) =(*)
if ((-10)<0) then (-(-10)) else (-10) =(**)
if True then (-(-10)) else (-10) =(***)
(-(-10)) =(****)
10
-}



{- Naming
Function and argument names must begin with
a lower-case letter. For example:

myFun     fun1     arg_2     x'
-}

-- Fun a b = a + b  -- not allowed

fUn1' x x' = x + x'
-- x    x
-- x'   x prime

-- two guards
-- a -> b means a function where the
--   input has type a
--   output has type b
absolute1 :: Int -> Int
absolute1 x
 | x<0 = -x
 | True = x

-- main = print (absolute1 (-4))  -- 4

-- parse error (ghc cannot build the tree from the program text)
-- "hello" !!
{-
      !!
    /   \
"hello"  ???
-}

-- type error (the tree was built, but some types don't match (input is not what is expected))
-- "hello" !! True

-- runtime error = exception
-- main = print ("hello" !! (-1))


{- Layout rule

-- OK
a = 10
b = 20
c = 30

-- BAD
a = 10
 b = 20
c = 30
       
-- BAD
 a = 10
b = 20
 c = 30

The layout rule avoids the need for explicit syntax to indicate the
grouping of definitions.

a = b + c              a = b + c
  where                  where  
    b = 1                  {b = 1;
    c = 2                   c = 2}
d = a * 2              d = a * 2

implicit grouping      explicit grouping
-}

-- more then two guards
signof :: Int -> Int
signof x
 | x >  0  = 1
 | x == 0  = 0
 | x <  -1  = -1       -- OPTION "careful",  might be more readable
-- | otherwise  = -1    -- OPTION "careless", more efficient, the program runs faster
 
--main =  print (signof (-8))   -- -1
