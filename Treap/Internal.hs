module Treap.Internal where

import           Control.Monad.Trans.State
import           Data.Foldable

-- | A "Treap" is both a Binary Tree and a Minimum Heap.
data Treap p a = E | N { lt_ :: Treap p a
                     , v_  :: a
                     , p_  :: p
                     , rt_ :: Treap p a }
  deriving (Eq, Ord, Show)

-- | The empty "Treap".
empty :: Treap p a
empty = E

-- | Check if the "Treep" contains the given element.
member :: Ord a => a -> Treap p a -> Bool
member _ E = False
member e (N l v _ r)
  | e < v     = member e l
  | e > v     = member e r
  | otherwise = True

-- | Insert an element with priority into the "Treap", replacing the original
-- one if exists.
insert :: Ord a => Ord p => a -> p -> Treap p a -> Treap p a
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

-- | Delete an element from the "Treap", returning a new "Treap".
delete :: Ord a => Ord p => a -> Treap p a -> Treap p a
delete _ E = E
delete e (N l x p r)
  | e < x     = N (delete e l) x p r
  | e > x     = N l x p (delete e r)
  | otherwise = merge l r
  where
    merge E r' = r'
    merge l' E = l'
    merge l'@(N a x' p' b) r'@(N c y' q' d)
      | p' < q'   = N a x' p' (merge b r')
      | otherwise = N (merge l' c) y' q' d

-- | Convert a list to a "Treap".
fromList :: Ord a => Ord p => [(a, p)] -> Treap p a
fromList = foldr (uncurry insert) E

-- | Look at the element with the lowest priority.
peek :: Treap p a -> Maybe (a, p)
peek E           = Nothing
peek (N _ e p _) = Just (e, p)

-- | Pop the element with the lowest priority.
pop :: Ord a => Ord p => Treap p a -> (Maybe (a, p), Treap p a)
pop E             = (Nothing, E)
pop t@(N _ e p _) = (Just (e, p), delete e t)

toAssoc :: Treap p a -> [(a, p)]
toAssoc = flip execState [] . go
  where
    go (N l e p r) = go r >> modify ((e, p) :) >> go l
    go E           = pure ()

instance Foldable (Treap p) where
  foldr _ a E            = a
  foldr f a (N l e _ r)  = foldr f (f e (foldr f a r)) l

  toList = flip execState [] . go
    where
      go (N l e _ r) = go r >> modify (e :) >> go l
      go E           = pure ()
