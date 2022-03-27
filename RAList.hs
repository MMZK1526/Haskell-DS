{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Random Access List with amortised O(1) prepending and O(log n) random
-- access.
-- It also provides popping (removing the first element), but mixing pushing &
-- popping would have O(log n) complexity in the worst case.

module Gadgets.RAList (
  RAList(Empty, (:<)), empty, fromList, head, length, update, update', 
  singleton, tail, toList, adjust, adjust', (!), (!?), (><), (=:), (=:!)
) where

import           Gadgets.RAList.Internal (
  RAList(..), empty, fromList, head, length, update, update', singleton, tail,
  toList, adjust, adjust', (!), (!?), (><), (=:), (=:!))
import           Data.Foldable (fold)
import           Gadgets.RAList.IsList ()
import           Prelude hiding (head, length, tail)

instance Functor RAList where
  fmap _ Empty     = Empty
  fmap f (x :< xs) = f x :< fmap f xs

instance Applicative RAList where
  pure = (:< Empty)

  Empty <*> _      = Empty
  (f :< fs) <*> xs = fmap f xs >< (fs <*> xs)

instance Semigroup (RAList e) where
  (<>) = (><)

instance Monoid (RAList e) where
  mempty = Empty

instance Monad RAList where
  xs >>= f = fold $ f <$> xs
