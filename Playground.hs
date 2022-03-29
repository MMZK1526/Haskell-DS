module Playground where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified RAList as RA
import qualified RBTree as RB
import qualified Treap as T

-- Demonstrate operations with "RAList".
playRAList :: IO ()
playRAList = do
  let list1 = RA.fromList [4 :: Integer, 5, 1, 4]
  putStrLn $ "Length of the list is " ++ show (length list1) ++ "."
  putStrLn "The elements are: "
  inspect list1
  putStrLn "Prepend some numbers..."
  let list2 = 1 RA.:< 1 RA.:< list1
  putStrLn "The new elements are: "
  inspect list2
  putStrLn "Now remove the head and modify the last element..."
  let list3 = RA.tail list2 RA.=:! 4 $ 5
  putStrLn "The new elements are: "
  inspect list3
  where
    inspect list = void . runMaybeT $ forM_ [0..] $ \i -> do
      value <- MaybeT . pure $ list RA.!? i
      lift . putStrLn $ "  Index " ++ show i ++ ": " ++ show value ++ "."

-- | Demonstrate operations with "RBTree".
playRBTree :: IO ()
playRBTree = do
  putStrLn "Tree sort:"
  let list = [1 :: Integer, 4, 2, 8, 5, 7, 6, 3, 9, 0]
  let tree = RB.fromList list
  putStrLn $ "  Original list: " ++ show list ++ "."
  putStrLn $ "  List length: " ++ show (length tree) ++ "."
  putStrLn $ "  Tree-sorted: " ++ show (RB.toList tree) ++ "."

-- | Demonstrate operations with "Treap".
playTreap :: IO ()
playTreap = do
  putStrLn "Start with an empty Treap."
  putStrLn "Insert 3 with priority 2."
  let treap1      = T.insert (3 :: Int) (2 :: Int) T.empty
  putStrLn "Insert 4 with priority 1."
  let treap2      = T.insert 4 1 treap1
  putStrLn "Insert 1 with priority 4."
  let treap3      = T.insert 1 3 treap2
  putStrLn "Insert 2 with priority 3."
  let treap4      = T.insert 2 4 treap3
  putStrLn $ "The treap with priorities: " ++ show (T.toAssoc treap4)
  let (e, treap5) = T.pop treap4
  putStrLn $ "Pop out the lowest priority: " ++ show e
  let treap6      = T.delete 2 treap5
  putStrLn $ "The treap after deleting 2: " ++ show (T.toAssoc treap6)
