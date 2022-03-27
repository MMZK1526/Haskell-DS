module Playground where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified RAList as RA

-- Demonstrate operations with RAList.
-- playRAList :: IO ()
playRAList = do
  let list = RA.fromList [4, 5, 1, 4]
  putStrLn $ "Length of the list is " ++ show (length list) ++ "."
  putStrLn "The elements are: "
  void . runMaybeT $ forM_ [0..] $ \i -> do
    value <- hoistMaybe $ list RA.!? i
    lift . putStrLn $ "  Index " ++ show i ++ ": " ++ show value ++ "."
