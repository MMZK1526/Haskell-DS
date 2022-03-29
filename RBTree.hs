{-# OPTIONS_GHC -Wno-orphans #-}

module RBTree (empty, fromList, toList, member, insert) where

import           Data.Foldable (toList)
import           RBTree.Internal (
  RBTree(..), empty, fromList, member, insert)
import qualified RBTree.Internal as RB

instance Foldable RBTree where
  foldr _ a E            = a
  foldr f a (N _ l v r)  = foldr f (f v (foldr f a r)) l

  toList = RB.toList
