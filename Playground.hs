module Playground where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified RAList as RA

-- Demonstrate operations with RAList.
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
      value <- hoistMaybe $ list RA.!? i
      lift . putStrLn $ "  Index " ++ show i ++ ": " ++ show value ++ "."
