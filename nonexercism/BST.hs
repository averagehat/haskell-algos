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
