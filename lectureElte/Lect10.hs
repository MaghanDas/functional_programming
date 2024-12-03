main :: IO()

-- Review trees

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
collectIn :: Tree Int -> [Int]
collectIn Leaf = []
collectIn (Node x left right) = collectIn left ++ [x] ++ collectIn right

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

-- end review


-- Sorting a list using Binary Search Trees BST

treesort :: [Int] -> [Int]
treesort = collect . toTree

toTree :: [Int] -> Tree Int
toTree [] = Leaf
toTree (x:xs) = insertT x (toTree xs)

insertT :: Int -> Tree Int -> Tree Int
insertT e Leaf = Node e Leaf Leaf
insertT e (Node x le ri)
  | e <= x = Node x (insertT e le) ri
  | e >  x = Node x le (insertT e ri)

collect :: Tree Int -> [Int]
collect Leaf = []
collect (Node x le ri) = collect le ++ [x] ++ collect ri

--main = print $ toTree [3,5,7,0]
-- Node 0 Leaf (Node 7 (Node 5 (Node 3 Leaf Leaf) Leaf) Leaf)
--main = print $ treesort [3,5,7,0] -- [0,3,5,7]

{-
insertT 3 (insertT 5 (insertT 7 (insertT 0 (toTree []))))
insertT 3 (insertT 5 (insertT 7 (insertT 0 Leaf)))

Leaf

insertT 3 (insertT 5 (insertT 7 (Node 0 Leaf Leaf)))
  0
 / \
Leaf Leaf

insertT 3 (insertT 5 (Node 0 Leaf (Node 7 Leaf Leaf))))
   0
 /   \
Leaf   7
      / \
    Leaf Leaf

 insertT 3 (Node 0 Leaf (Node 7 (Node 5 Leaf Leaf) Leaf))
   0
 /   \
Leaf  7
     / \
    5  Leaf
   / \
Leaf Leaf

 Node 0 Leaf (Node 7 (Node 5 (Node 3 Leaf Leaf) Leaf) Leaf)
    0
  /  \
Leaf  7
     / \
    5  Leaf
   / \
  3   Leaf
 /  \
Leaf Leaf

collecting inorder : [0,3,5,7]
-}

-- operations with trees

-- map :: (a -> b) -> [a] -> [b]
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node a t1 t2) = Node (f a) (mapTree f t1) (mapTree f t2)

--main = print tree1
-- Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) Leaf
--main = print $ mapTree (\x -> fromIntegral x / 2) tree1
-- Node 2.0 (Node 1.0 (Node 0.5 Leaf Leaf) (Node 1.5 Leaf Leaf)) Leaf


-- foldr :: (a -> b -> b)      -> b -> [a]    -> b
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree n u Leaf = u
foldTree n u (Node a t1 t2) = n a (foldTree n u t1) (foldTree n u t2)

--main = print $ foldTree (\ a b c -> a*b*c) 1 tree1 -- 24



-- Various tree types

data Tree2 a = Node2 a (Tree2 a) (Tree2 a) | 
               Leaf2 a 
               deriving (Show)
{-
  Tree2          Tree
   2              2    
  / \           /   \   
 1   3         1      3  
/\   / \      /\     / \ 
4 5  6  7    4  5    6  7
            /\  /\  /\   /\
            L L L L L L L  L
-}


aTree2 :: Tree2 Int
aTree2 = Node2 2 (Node2 1 (Leaf2 4)(Leaf2 5)) (Node2 3 (Leaf2 6)(Leaf2 7))

--main = print aTree2
-- Node2 2 (Node2 1 (Leaf2 4) (Leaf2 5)) (Node2 3 (Leaf2 6) (Leaf2 7))

bTree2 :: Tree2 Int
bTree2 = Node2 2 (Leaf2 2) (Node2 3 (Leaf2 1) (Leaf2 1))

--main = print bTree2
-- Node2 2 (Leaf2 2) (Node2 3 (Leaf2 1) (Leaf2 1))


data Tree3 a b = Node3 a (Tree3 a b) (Tree3 a b) |
                 Leaf3 b
                 deriving (Show)
{-
Tree3 Bool Int
   True
    / \
   1  False
      / \
     2   3
-}

aTree3 :: Tree3 Bool Int
aTree3 = Node3 True (Leaf3 1) (Node3 False (Leaf3 2) (Leaf3 3))

--main = print aTree3
-- Node3 True (Leaf3 1) (Node3 False (Leaf3 2) (Leaf3 3))

bTree3 :: Tree3 Int Float
bTree3 = Node3 2 (Node3 1 (Leaf3 1.1) (Leaf3 2.5)) (Node3 3 (Leaf3 3.0) (Leaf3 6.9))

--main = print $ bTree3
-- Node3 2 (Node3 1 (Leaf3 1.1) (Leaf3 2.5)) (Node3 3 (Leaf3 3.0) (Leaf3 6.9))

sumLeaves :: (Tree3 Int Float) -> Float
sumLeaves (Leaf3 y) = y
sumLeaves (Node3 x le ri) = sumLeaves le + sumLeaves ri

--main = print $ sumLeaves bTree3 --13.5


data Tree4 a = Node4 a (Tree4 a) (Tree4 a) (Tree4 a) | 
               Leaf4 
               deriving (Show)

{-  10
  /  |  \
  1  2   3
 /|\      
7 8 9  
-}

aTree4 :: Tree4 Int
aTree4 = Node4 10 (Node4 1 (Node4 7 Leaf4 Leaf4 Leaf4)(Node4 8 Leaf4 Leaf4 Leaf4)(Node4 9 Leaf4 Leaf4 Leaf4)) 
                  (Node4 2 Leaf4 Leaf4 Leaf4) 
                  (Node4 3 Leaf4 Leaf4 Leaf4)

--main = print aTree4
-- Node4 10 (Node4 1 (Node4 7 Leaf4 Leaf4 Leaf4) (Node4 8 Leaf4 Leaf4 Leaf4) (Node4 9 Leaf4 Leaf4 Leaf4)) 
--          (Node4 2 Leaf4 Leaf4 Leaf4) (Node4 3 Leaf4 Leaf4 Leaf4)


-- Rose trees (filesystems)
data Tree5 a = Node5 a [Tree5 a] deriving (Show)

{-
    1
  / | \
 2  4  5
 |    / \
 3   6   7
        /|\  \
       8 9 10 11
              |
              12
-}

roseTree :: Tree5 Int
roseTree = Node5 1
  [ Node5 2 [ Node5 3 [] ]
  , Node5 4 []
  , Node5 5
      [ Node5 6 []
      , Node5 7
          [ Node5 8 []
          , Node5 9 []
          , Node5 10 []
          , Node5 11 [ Node5 12 [] ]
          ]
      ]
  ]

--main = print $ roseTree
-- Node5 1 [Node5 2 [Node5 3 []],Node5 4 [],Node5 5 [Node5 6 [],Node5 7 [Node5 8 [],Node5 9 [],Node5 10 [],Node5 11 [Node5 12 []]]]]

buildRose :: a -> Tree5 a
buildRose a = Node5 a []

--main = print $ buildRose 5 -- Node5 5 []


-- single child trees = lists
data Tree6 a = Node6 a (Tree6 a) | Leaf6 deriving (Show)

-- Node6 is (:)
-- Leaf6 is []
-- Node6 x1 (Node6 x2 (Node6 x3 Leaf6)) =
-- x1 : x2 : x3 : []

aTree6 :: Tree6 Int
aTree6 = Node6 1 (Node6 2 (Node6 3 Leaf6))

--main = print aTree6
-- Node6 1 (Node6 2 (Node6 3 Leaf6))


-- example usage of trees
data Exp = Num Int | Add Exp Exp | Mul Exp Exp

{-
    +
   / \       (2*2)+3
  *   3
 / \
2   2

    *
   / \        2*(2+3)
  2   +
     / \
    2   3
-}

exp1, exp2 :: Exp
exp1 = Add (Mul (Num 2) (Num 2)) (Num 3)
exp2 = Mul (Num 2) (Add (Num 2) (Num 3))

-- meaning of an expression
eval :: Exp -> Int
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

--main = print (eval exp1, eval exp2) -- (7,10)



-- Classes and Instances

-- Show class


-- Show is called a type class
-- converting values to readable strings
-- Customized intances of Show class
-- show :: Show a => a -> String
-- this means that show is a function from a to String
-- for any type a, provided that a has a Show instance


data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Show Days where
  show :: Days -> String
  show Mon = "M"
  show Tue = "T"
  show Wed = "W"
  show Thu = "T"
  show Fri = "f"
  show Sat = "S"
  show Sun = "s"

--main = print (Mon, Sun) -- (M,s)


instance Show Exp where
  show (Num n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

--main = print (exp1, exp2) -- (((2*2)+3),(2*(2+3)))

-- in ghci, do:
-- :i Show


data Multiple = Multiple Int Int Int -- deriving Show -- error duplicate instance

-- writing a custom printing function
printMultiple :: Multiple -> String
printMultiple (Multiple a b c) = show a ++ "*" ++ show b ++ "*" ++ show c

m1, m2 :: Multiple
m1 = Multiple 1 2 3
m2 = Multiple 4 5 6
--main = print $ printMultiple m1 -- "1*2*3"


-- write Show instance for the custom types
-- show :: a -> String

instance Show Multiple where
  show x@(Multiple a b c) = printMultiple x

--main = print $ m1 -- 1*2*3
--main = print $ show m1 ++ " " ++ show m2 -- "1*2*3 4*5*6"



-- Q type for rational values and its instance operations
-- rational numbers: numerator and denominator

data Q = Q { num :: Int , denom :: Int } -- deriving Show

-- simplify makes the numerator and denominator relative primes and
-- the denominator positive 
-- there is only one such representation of a rational number

simplify (Q { num , denom }) =
  Q { num = sgn * abs num `div` n , denom = abs denom `div` n }
  where
    n = gcd (abs num) (abs denom)
    sgn | num>0 && denom>0 || num<0 && denom<0 = 1
        | num<0 && denom>0 || num>0 && denom<0 = -1
        | num == 0 && denom /= 0               = 0
        | denom == 0                           = error "denominator cannot be 0"

-- to make printing work, we need to write "deriving Show" above, or
-- define our own Show instance

instance Show Q where
  show :: Q -> String
  show r = show (num r) ++ "/" ++ show (denom r)

--main = print $ Q 4 6 -- 4/6
--main = print $ simplify $ Q 4 6 -- 2/3
--main = print $ Q (-1) 2 -- -1/2
--main = print $ Q 2 3 -- 2/3
--main = print $ Q 2 8 -- 2/8



-- Num typeclass

-- ghci> :i Num

instance Num Q where
  (+) :: Q -> Q -> Q
  Q a b + Q c d = simplify $ Q (a*d + c*b) (b*d)
  (*) :: Q -> Q -> Q
  Q a b * Q c d = simplify $ Q (a*c) (b*d)
  abs :: Q -> Q
  abs (Q a b)   = Q (abs a) (abs b)
  fromInteger :: Integer -> Q
  fromInteger a = Q (fromInteger a) 1
  negate :: Q -> Q
  negate (Q a b) = simplify $ Q (-a) b
  signum :: Q -> Q
  signum         = flip Q 1 . signum . num . simplify
  
--main = print $ Q 1 3 + Q 4 3 -- 5/3
--main = print $ Q 5 3 * Q 2 3 -- 10/9
--main = print $ abs $ Q 1 3 -- 1/3
--main = print $ abs $ Q 1 (-3) -- 1/3
--main = print (fromInteger 3 :: Q) -- 3/1
--main = print $ negate (Q 1 2) -- -1/2
--main = print $ Q 1 3 - Q 2 3 -- -1/3
--main = print $ signum $ Q 1 3 -- 1/1
--main = print $ signum $ Q (-1) 3 -- -1/1
--main = print $ signum $ Q 0 3 -- 0/1



-- Eq typeclass

-- ghci> :i Eq

-- it is enough to compare the components of the simplified numbers directly
-- or cross multiplication

instance Eq Q where
  (==) :: Q -> Q -> Bool
  u == v = a == c && b == d
    where
      Q a b = simplify u
      Q c d = simplify v

--main = print $ Q 1 3 == Q 3 9 -- True
--main = print $ Q 1 3 == Q 2 3 -- False
--main = print $ Q 0 3 == Q 0 2 -- True


-- Ord typeclass

-- ghci> :i Ord
-- ghci> :t compare
-- ghci> :i Ordering

-- because d and c are positive, when a/b > c/d is equivalent to a*d > c*b

instance Ord Q where
  compare :: Q -> Q -> Ordering
  compare u v = compare (a * d) (c * b)
    where
      Q a b = simplify u
      Q c d = simplify v

--main = print $ Q 2 100 > Q 1 100 -- True
--main = print $ Q 2 100 > Q 1 50 -- False
--main = print $ Q 2 100 == Q 1 50 -- True



-- More examples
-- Num for Bool

-- we treat False as 0, True as 1, and addition leaves only the
-- remainder (also called modulo 2 addition)

instance Num Bool where
  False + b = b
  True  + b = not b
  False * b = False
  True  * b = b
  abs    b = b
  signum b = b
  negate b = b
  fromInteger = odd

--main = print (1 :: Bool) -- True
--main = print (2 :: Bool) -- False
--main = print (3 :: Bool) -- True
--main = print (4 :: Bool) -- False
--main = print (1 + 2 :: Bool) -- True



-- instances for trees

data BTree a = BNode a (BTree a) (BTree a) | BLeaf

-- this is a conditional instance, it relies on Show a
-- if the base type a has Show instance then it can build 
-- show instance for (BTree a)

instance Show a => Show (BTree a) where
  show BLeaf = "L"
  show (BNode a t1 t2) = "(N " ++ show a ++ " " ++ show t1 ++ " " ++ show t2 ++ ")"

--main = print $ BNode 1 (BNode 2 BLeaf BLeaf) BLeaf -- (N 1 (N 2 L L) L)
--main = print $ BNode 1 BLeaf (BNode 2 BLeaf BLeaf) -- (N 1 L (N 2 L L))
--main = print (BLeaf :: BTree Int) -- L


instance Eq a => Eq (Tree a) where
  Leaf         == Leaf            = True
  Node a t1 t2 == Node a' t1' t2' = a == a' && t1 == t1' && t2 == t2'
  _            == _               = False

--main = print $ Node 1 (Node 2 Leaf Leaf) Leaf == Node 1 (Node 2 Leaf Leaf) Leaf -- True
--main = print $ Node 1 (Node 2 Leaf Leaf) Leaf == Node 1 (Node 3 Leaf Leaf) Leaf -- False
--main = print $ Node 1 Leaf (Node 2 Leaf Leaf) == Leaf -- False
--main = print $ (Leaf :: Tree Int) == (Leaf :: Tree Int) -- True



-- Functor typeclass

-- ghci> :i Functor
-- ghci> :t fmap

-- map :: (a -> b) -> [a] -> [b]
-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree f Leaf = Leaf
-- mapTree f (Node a t1 t2) = Node (f a) (mapTree f t1) (mapTree f t2)

-- main = print $ mapTree (+1) (Node 1 (Node 2 Leaf Leaf) Leaf)
-- Node 2 (Node 3 Leaf Leaf) Leaf

-- Functor t means that we can define a map-like function for the t type

instance Functor Tree where
  fmap = mapTree

--main = print $ fmap (+1) (Node 1 (Node 2 Leaf Leaf) Leaf)
-- Node 2 (Node 3 Leaf Leaf) Leaf