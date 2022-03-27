module RBTree.Internal where

import           Data.List

-- | Red-Black Tree.
data RBTree a = E | N { colour_ :: Colour
                      , lt_     :: RBTree a
                      , value_  :: a
                      , rt_     :: RBTree a }
  deriving (Eq, Ord, Show)

-- | The colour of a "RBTree".
data Colour = R | B
  deriving (Enum, Eq, Ord, Show)

-- | Check if a "RBTree" is valid, namely 1) rooted with black and 2) any path
-- from the root to a leaf contains the same number of black nodes.
isValid :: RBTree a -> Bool
isValid E = True
isValid tree
  | colour_ tree == R = False
  | otherwise         = null . tail . nub $ pathCounts tree
  where
    pathCounts E = [0]
    pathCounts (N c l _ r) = (+ fromEnum c) <$> (pathCounts l ++ pathCounts r)

-- | Check if the "RBTree" contains the given element.
member :: Ord a => a -> RBTree a -> Bool
member _ E           = False
member e (N _ l v r)
  | e < v     = member e l
  | e > v     = member e r
  | otherwise = True

-- | Insert an element into the "RBTree", replacing the original one if exists.
insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = blacken $ go tree
  where
    go E = N R E x E
    go (N c l e r)
      | x < e     = balance $ N c (go l) e r
      | x > e     = balance $ N c l e (go r)
      | otherwise = N c l x r

-- | Turn the node black.
blacken :: RBTree a -> RBTree a
blacken tree@N {} = tree { colour_ = B }
blacken tree      = tree

-- | Balance an "RBTree".
balance :: RBTree a -> RBTree a
balance tree = case tree of
  N B (N R (N R a x b) y c) z d -> N R (N B a x b) y (N B c z d)
  N B (N R a x (N R b y c)) z d -> N R (N B a x b) y (N B c z d)
  N B a x (N R (N R b y c) z d) -> N R (N B a x b) y (N B c z d)
  N B a x (N R b y (N R c z d)) -> N R (N B a x b) y (N B c z d)
  _                             -> tree
