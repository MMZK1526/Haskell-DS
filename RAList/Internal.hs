{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module RAList.Internal
  (RAList(Empty, (:<)), empty, fromList, head, length, update, update',
   singleton, tail, toList, adjust, adjust', (!), (!?), (><), (=:), (=:!)
) where

import           Control.Monad (ap)
import           Data.Bifunctor (second)
import           Data.Foldable (Foldable(toList))
import           Data.Maybe (fromJust, isJust)
import           Prelude hiding (head, length, tail)

-- | Random Access List with amortised O(1) prepending and O(log n) random
-- access.
data RAList a = RAList Int! [Maybe (Tree a)]
  deriving (Eq, Show)

-- | A bidirectional pattern synonym matching an empty @RAList@.
pattern Empty :: RAList a
pattern Empty = RAList 0 []

-- | A bidirectional pattern synonym for a non-empty @RAList@. Equivalent to
-- (:) for ordinary lists. Amortised O(1).
infixr 5 :<
pattern (:<) :: a -> RAList a -> RAList a
pattern x :< xs <- (raView -> x :<: xs)
  where
    x :< xs = x `cons` xs

instance Foldable RAList where
  foldr _ a Empty      = a
  foldr f a (x :< xs)  = f x (foldr f a xs)

  toList (RAList _ ts) = concat $ flatten . fromJust <$> filter isJust ts

-- | An empty @RAList@.
empty :: RAList a
empty = Empty

-- | Construct a @RAList@ from a [].
fromList :: [a] -> RAList a
fromList = foldr (:<) Empty

-- | Gets the first element from a @RAList@. Amortised O(1).
head :: RAList a -> a
head = fst . fromJust . raSplit

-- | Gets the length of a @RAList@. O(1).
length :: RAList a -> Int
length (RAList l _) = l

-- | @RAList@ from a singleton.
singleton :: a -> RAList a
singleton a = RAList 1 [Just $ Leaf a]

-- | Gets but the first element of a @RAList@. Amortised O(1).
tail :: RAList a -> RAList a
tail = snd . fromJust . raSplit

-- | Accesses the ith element of a @RAList@. O(log n).
infixl 9 !
(!) :: RAList a -> Int -> a
(!) = (fromJust .) . (!?)

-- | Safely accesses the ith element of a @RAList@. O(log n).
infixl 8 !?
(!?) :: RAList a -> Int -> Maybe a
RAList _ trees !? ix = go trees ix
  where
    go (Nothing : ts) i = go ts i
    go (Just t  : ts) i
      | i < s     = Just $ indexAt t i
      | otherwise = go ts (i - s)
      where
        s = size t
    go _              _ = Nothing

-- | Concatenates two @RALists@. O(n).
infixr 5 ><
(><) :: RAList a -> RAList a -> RAList a
Empty >< ys     = ys
(x :< xs) >< ys = x :< (xs >< ys)

-- | Infix version of @update@.
-- Example: @ ral =: 3 $ 5 @ sets the third element to five.
-- It will produce the same RAList if the index is out of bound.
--
infixl 3 =:
(=:) :: RAList a -> Int -> a -> RAList a
(=:) = update


-- | The strict version of (=:). O (log n)
infixl 3 =:!
(=:!) :: RAList a -> Int -> a -> RAList a
(=:!) = update'

-- | Updates the RAList at the given index. If the index is out of bound,
-- nothing happens. O(log n).
update :: RAList a -> Int -> a -> RAList a
update = ((. const) .) . flip . adjust

-- | The strict version of "update". O(log n).
update' :: RAList a -> Int -> a -> RAList a
update' = ((. const) .) . flip . adjust'

-- | Adjusts the RAList at the given index by an updating function. If the
-- index is out of bound, nothing happens. O(log n).
adjust :: RAList a -> (a -> a) -> Int -> RAList a
adjust (RAList l trees) f ix = RAList l $ go trees ix
  where
    go (Nothing : ts) i = Nothing : go ts i
    go (Just t  : ts) i
      | i < s     = Just (treeUpdate t f i) : ts
      | otherwise = Just t : go ts (i - s)
      where
        s = size t
    go _              _ = []

-- | The strict version of "adjust". O(log n).
adjust' :: RAList a -> (a -> a) -> Int -> RAList a
adjust' = (. ap seq) . adjust

-- | Prepends an element to a @RAList@.
--
-- We can prove that the amortised cost of "cons" is O(1):
--
-- For any @RAList@ ts, define @size ts := length $ filter isJust ts@.
--
-- Since node is O(1), the real cost for a @"cons"@ operation is l + 1, where l
-- is the number of "Just"s at the beginnning of the @RAList@.
--
-- Define @amorCost ts = 2@. Assume size ts = t. Then if ts starts with
-- "Nothing", we have @cost ts = 1 <= 2 + t - (t + 1) = amorCost ts + size ts -
-- size (cons"a ts)@. Otherwise, f ts starts with l "Just"s, we have @cost ts =
-- l + 1 <= 2 + t - (t - l + 1) = amorCost ts + size ts - size (cons a ts)@.
-- Therefore, we have @realCost <= amortisedCost@ at all time, thus in n
-- consecutive applications of @"cons"@, the total cost is no greater than 2n,
-- and the amortised cost for each operation is 2n / n = O(1).
cons :: a -> RAList a -> RAList a
cons a (RAList l trees) = RAList (l + 1) $ go (Leaf a) trees
  where
    go t []             = [Just t]
    go t (Nothing : ts) = Just t : ts
    go t (Just t' : ts) = Nothing : go (node t t') ts

-- | Splits a RAList into its first element and rest.
raSplit :: RAList a -> Maybe (a, RAList a)
raSplit (RAList l trees) = second (RAList (l - 1)) <$> go Nothing trees
  where
    go (Just (Leaf a)) ts      = Just (a, ts)
    go (Just (Tree _ t t')) ts = go (Just t) (Just t' : ts)
    go Nothing [Just t]        = go (Just t) []
    go Nothing (Just t : ts)   = go (Just t) (Nothing : ts)
    go Nothing (Nothing : ts)  = go Nothing ts
    go _ _                     = Nothing

-- | An external binary tree.
data Tree a = Leaf a | Tree Int (Tree a) (Tree a)
  deriving (Eq, Show)

-- | Calculates the size of a Tree.
size :: Tree a -> Int
size (Leaf _)     = 1
size (Tree n _ _) = n

-- | Combines two Trees.
node :: Tree a -> Tree a -> Tree a
node l r = Tree (size l + size r) l r

-- | Gets element at the given index of a Tree.
indexAt :: Tree a -> Int -> a
indexAt (Leaf x)     0 = x
indexAt (Tree _ l r) i
  | i < ls    = indexAt l i
  | otherwise = indexAt r (i - ls)
  where
    ls = size l
indexAt _            _ = undefined

-- | Updates the ith element of the Tree.
treeUpdate :: Tree a -> (a -> a) -> Int -> Tree a
treeUpdate (Leaf a)     f 0 = Leaf $ f a
treeUpdate (Leaf a)     _ _ = Leaf a
treeUpdate (Tree n l r) f i
  | i < ls    = Tree n (treeUpdate l f i) r
  | otherwise = Tree n l (treeUpdate r f (i - ls))
  where
    ls = size l

-- | flattens a Tree to a [].
flatten :: Tree a -> [a]
flatten tree = go tree []
  where
    go (Leaf a) []       = [a]
    go (Leaf a) (t : ts) = a : go t ts
    go (Tree _ l r) ts   = go l (r : ts)

data RAView a = NIL | a :<: RAList a

raView :: RAList a -> RAView a
raView as = case raSplit as of
  Nothing      -> NIL
  Just (a, as') -> a :<: as'
