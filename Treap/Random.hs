module Treap.Random where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Random
import           Treap.Internal (Treap)
import qualified Treap.Internal as T

-- | Insert an element with a random priority into a "Treap".
insert :: MonadIO m => Ord a => a -> Treap Int a -> m (Treap Int a)
insert e treap = do
  gen <- getStdGen
  let (p, gen') = random gen
  setStdGen gen'
  return $ T.insert e p treap

-- | Turn a list of elements into a "Treap" with randomised priorities.
fromList :: MonadIO m => Ord a => [a] -> m (Treap Int a)
fromList = foldM (flip insert) T.empty
