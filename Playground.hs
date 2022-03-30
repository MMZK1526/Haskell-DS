module Playground where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified RAList as RA
import qualified RBTree as RB
import qualified QuickSort as Q
import qualified Treap as T
import qualified Treap.Random as TR

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

-- | Sorting with randomised "Treap".
--
-- With a perfectly balanced tree, our Treap would have a depth of 7.
-- Since it is randomised, however, the depth is most likely larger, and it is
-- usually somewhere between 14 and 18.
--
-- Try it :)
playRTreap :: IO ()
playRTreap = do
  putStrLn "Sort [127, 126..1] with a randomised Treap..."
  treap <- TR.fromList [127 :: Int, 126..1]
  putStrLn $ "The depth of the Treap is " ++ show (T.depth treap) ++ "."
  putStrLn $ "Sorted: " ++ show (T.toList treap) ++ "."

-- | Use quick-sort.
playQuickSort :: IO ()
playQuickSort = do
  putStrLn "Sort [127, 126..1] with quick-sort."
  let result = Q.quickSort [127 :: Int, 126..1]
  putStrLn $ "Sorted: " ++ show result ++ "."

-- | Use quick-sort.
playQuickSelect :: IO ()
playQuickSelect = do
  let list   = [127 :: Int, 126..1]
  putStrLn "Find the minimum of [127, 126..1] with quick-select."
  let result1 = Q.quickSelect 0 list
  putStrLn $ "Result: " ++ show result1 ++ "."
  putStrLn "Find the tenth smallest element of [127, 126..1] with quick-select."
  let result2 = Q.quickSelect 10 list
  putStrLn $ "Result: " ++ show result2 ++ "."
  putStrLn "Find the maximum of [127, 126..1] with quick-select."
  let result3 = Q.quickSelect (length list - 1) list
  putStrLn $ "Result: " ++ show result3 ++ "."

