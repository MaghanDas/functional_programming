import Data.Map.Internal.Debug (balanced)
-- import qualified Control.Applicative as tree
-- import Data.Binary.Get ()
--import qualified Control.Applicative as not

-- defining tree 

data Tree a = Empty        -- here Empty represents empty tree. 
                |Node a (Tree a ) (Tree a)   -- represents a node with a value and two subtrees. 
                deriving (Show)

-- Tree a -> type constructor , a is type of data (eg. int, string)
-- Empty -> constructore for empty tree
-- Node a (Tree a ) ( Tree a ) -> represents a tree node with value type a  and left, right subtree..

{-
       10
     /      \ 
  5         20 
           /     \
       15       25 
-}

exampleTree :: Tree Int
exampleTree = Node 10 
                         (Node 5 Empty Empty) 
                         (Node 20(Node 15 Empty Empty)(Node 25 Empty Empty))

exampleTree2 :: Tree a 
exampleTree2 = Empty
-- main = print $ exampleTree -- Node 10 (Node 5 Empty Empty) (Node 20 (Node 15 Empty Empty) (Node 25 Empty Empty))

-- Tree traversels 
-- pre-order   (Root,  Left, Right)
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node value left right ) = [value] ++ preOrder left ++ preOrder right 
-- main = print $ preOrder exampleTree -- [10,5,20,15,25]

-- in-order     (Left,  Root, Right)
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node value left right ) =inOrder left ++  [value]  ++ inOrder right 

-- main = print $ inOrder exampleTree -- [5,10,15,20,25]

-- post-order (Left, Right, Root)
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node value left right ) =  postOrder left ++ postOrder right ++ [value] 
-- main = print $ postOrder exampleTree ---- [5,15,25,20,10]

 -- TREE - OPERATIONS 
 
 -- Checking if tree is empty or not..
isEmpty :: Tree a -> Bool 
isEmpty Empty = True 
isEmpty _ = False 

-- main = print $ isEmpty exampleTree2 -- True 
-- main = print $ isEmpty exampleTree -- false 

-- Calculating the height of a tree..
heightTree :: Tree a -> Int 
heightTree Empty = 0 
heightTree (Node value left right ) = 1 + max (heightTree left) (heightTree right)

-- main = print $ heightTree exampleTree -- 3 

-- Counting Nodes in a Tree 
countNodes :: Tree a -> Int 
countNodes Empty = 0 
countNodes (Node value left right) = 1+ countNodes left + countNodes right

-- main = print $ countNodes exampleTree -- 5

-- Search for an element 
searchElements :: Tree Int -> Int -> Bool 
searchElements Empty _ = False 
searchElements (Node value left right) n 
    | n == value =True 
    | otherwise = searchElements left n || searchElements right n 
    {-
       10
     /      \ 
  5         20             -- same exampleTree, we have been using so far.... 
           /     \
       15       25 
-}

-- main = do 
--     print $ searchElements exampleTree 10 -- True
--     print $ searchElements exampleTree (-9) -- False 
--     print $ searchElements exampleTree 15 -- True 
--     print $ searchElements exampleTree 4 -- False 

-- create this tree: [10, 5, 15, 3, 7, 12, 18]:  =>  make 10 as root and then if element is smaller it goes to left subTree otherwise Right if greater than root 
exampleTree3 :: Tree Int 
exampleTree3 = Node 10
                            (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
                            (Node 15(Node 12 Empty Empty) (Node 18 Empty Empty))
                        -- write a function to find minimum value in tree..
minimumElement :: Tree Int -> Int 
minimumElement Empty = error "Tree is Empty"
minimumElement(Node value Empty right ) = value 
minimumElement (Node value left right ) = minimumElement left
-- main = print $ minimumElement exampleTree3

balancedTree :: Tree Int -> Bool 
balancedTree Empty = True 
balancedTree (Node _ left right) = abs(heightTree left - heightTree right) <=1 && balancedTree left && balancedTree right 


main = print $ balancedTree exampleTree3

-- if the tree is not bst, we ned to check in left subtree and right subtree both .....
{-}
data Tree a = Empty                     -- Represents an empty tree
            | Node a (Tree a) (Tree a)  -- Represents a node with a value and two subtrees
            deriving (Show)

-- Example tree:
--       10
--      /  \
--     20    5
--         /  \
--        15   2

exampleTree :: Tree Int
exampleTree = Node 10 
                  (Node 20 Empty Empty) 
                  (Node 5 (Node 15 Empty Empty) (Node 2 Empty Empty))

-- Function to find the minimum element in a non-BST tree
minimumElement :: (Ord a, Show a) => Tree a -> a
minimumElement Empty = error "Tree is empty"
minimumElement (Node value Empty Empty) = value  -- Leaf node
minimumElement (Node value left Empty) = min value (minimumElement left)  -- Only left subtree
minimumElement (Node value Empty right) = min value (minimumElement right) -- Only right subtree
minimumElement (Node value left right) =
    min value (min (minimumElement left) (minimumElement right))  -- Both subtrees exist

-- Main function
main :: IO ()
main = print $ minimumElement exampleTree  -- Expected output: 2
-}