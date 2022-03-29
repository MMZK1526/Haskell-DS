module Treap where

data Treap a = E | N { lt_ :: Treap a
                     , v_  :: a
                     , p_  :: Int
                     , rt_ :: Treap a }
  deriving (Eq, Ord, Show)

-- | The empty "Treap".
empty :: Treap a
empty = E

-- | Check if the "Treep" contains the given element.
member :: Ord a => a -> Treap a -> Bool
member _ E = False
member e (N l v _ r)
  | e < v     = member e l
  | e > v     = member e r
  | otherwise = True

-- | Insert an element with priority into the "RBTree", replacing the original
-- one if exists.
insert :: Ord a => a -> Int -> Treap a -> Treap a
insert e p E = N E e p E
insert e p (N l x q r)
  | e < x     = rotl (insert e p l) x q r
  | e > x     = rotr l x q (insert e p r)
  | otherwise = N l e p r
  where
    rotl (N a x' p' b) y' q' c
      | p' < q' = N a x' p' (N b y' q' c)
    rotl a y' q' b = N a y' q' b
    rotr a y' q' (N b x' p' c)
      | p' < q' = N (N a y' q' b) x' p' c
    rotr a y' q' b = N a y' q' b
