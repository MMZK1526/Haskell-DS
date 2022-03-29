module Treap ( 
  empty, delete, fromList, insert, member, peek, pop, toAssoc, toList) where

import           Treap.Internal 
  (empty, delete, fromList, insert, member, peek, pop, toAssoc)
import           Data.Foldable (toList)
