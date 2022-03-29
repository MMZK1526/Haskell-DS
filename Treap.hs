module Treap ( 
  empty, delete, depth, fromList, insert, member, peek, pop, toAssoc, toList
  ) where

import           Treap.Internal 
  (empty, delete, depth, fromList, insert, member, peek, pop, toAssoc)
import           Data.Foldable (toList)
