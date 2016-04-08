{- Create a balanced binary tree from a list of elements.
Note that this is not a BST, ordering of elements does not matter, but
maximum height is log2(N) -}

data Tree a = Node Integer (Tree a) a (Tree a) | Leaf
 deriving (Show, Eq)
{- Basically, you know that a node has height one greater than it's children.
 you make a recursive call to insert as you normally do. Always insert by replacing a leaf
if there is one.
If a node your inserting into doesn't have a leaf, recursively call insert on the side that
has a lower height, and return the new node with that insert attached as you would normally.
The height of this node will be one greater than height of its *tallest child*. -}

-- annotate function with assumed type before starting implementation for compiler help
foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs
  where
    insert :: a -> Tree a -> Tree a 
    insert x Leaf = empty x 0
    insert x (Node _ Leaf v Leaf)  = Node 1 (empty x 0) v Leaf
    --insert x (Node _ Leaf v r@(Node h _ _ _))  = -- Not needed, always adds on left first
    insert x (Node _ l@(Node h _ _ _) v Leaf)  = Node (h+1) l v (empty x 0)  -- note that height here will always be 1 
    insert x (Node h l@(Node lh _ _ _) v r@(Node rh _ _ _)) =
      if (lh <= rh)  then
        let n@(Node h' _ _ _) = (insert x l)
            in Node (max (h'+ 1) (rh + 1)) n v r
      else
        let n@(Node h' _ _ _) = (insert x r)
              in Node (max (h' + 1) (lh + 1)) l v n
               
    empty x h = Node h Leaf x Leaf

main = do
  putStrLn $ show $ foldTree ([] :: [Int])
  putStrLn $ show $ foldTree "ABCDEFGHIJK"

data Tree a = Leaf | Node a (Tree a) (Tree a)
empty v = Node v Leaf Leaf
-- left, parent, right
flatten :: Tree a -> [a]
flatten n@(Node x l r) = (x : (flatten l)) ++ (flatten r)
flatten Leaf = [] 


main2 = do 
  let res = flatten $ (Node 1 (Node 2 (empty 3) (empty 4)) (Node 5 Leaf (empty 6)) )
  print res


--     1
--   /  \  
--  7    2  
-- |    /
-- 8  3
-- |
-- 9

-- rightmost nodes followed by all rightmost noes
-- starting with a depth of 0, all rightmost nodes under the last depth followed by 
-- all next-most right nodes under that depth . . . 

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

rightSight :: Tree a -> [a]
rightSight t = snd $ f t 0 0
  where
    f :: Tree a -> Int -> Int -> (Int, [a])
    f Leaf dsf _ = (dsf, [])
    f n@(Node v l r) dsf cd = if (cd' > dsf) then (maxd, (v:rest)) else (maxd, rest)
      where 
        (ldsf, lrest) = f r (max dsf cd') cd'
        (rdsf, rrest) = f l ldsf cd'
        rest = lrest ++ rrest
        cd' = (cd + 1)
        maxd = max ldsf rdsf


empty x = Node x Leaf Leaf

mainRightSight = do
  print $ rightSight (empty 1)
  let n = (Node 1 (Node 7 (Node 8 (empty 9) Leaf) Leaf)   (Node 2 (empty 3) Leaf))
  print $ rightSight n
  print $ rightSight $ (Node 1 (Node 2 (empty 3) Leaf) Leaf)
  let n2 = (Node 1 (Node 2 Leaf (empty 5)) (Node 3 Leaf (empty 4)))
  print $ rightSight n2
  
--   1            <---
-- /   \
--2     3         <---
-- \     \
--  5     4       <---

