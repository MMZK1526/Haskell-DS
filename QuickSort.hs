{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module QuickSort where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Array.ST
import           Data.Functor
import           System.Random

quickSort :: Ord a => [a] -> [a]
quickSort xs = runST $ do
  arrST <- newArr (0, length xs - 1) xs
  let swap i j      = do
        vi <- readArray arrST i
        vj <- readArray arrST j
        writeArray arrST i vj
        writeArray arrST j vi
  let partition p q = do
        x <- readArray arrST p
        let loop i j
              | i > j     = swap p j $> j
              | otherwise = do
                u <- readArray arrST i
                if u < x then loop (i + 1) j else swap i j >> loop i (j - 1)
        loop (p + 1) q
  let qSort i j
        | i >= j    = return ()
        | otherwise = do
          gen <- unsafeIOToST getStdGen
          let (p, gen') = randomR (i, j) gen
          unsafeIOToST $ setStdGen gen'
          unless (i == p) $ swap i p
          k   <- partition i j
          qSort i (k - 1)
          qSort (k + 1) j
  qSort 0 (length xs - 1)
  getElems arrST
  where
    newArr = newListArray :: (Int, Int) -> [a] -> ST s (STArray s Int a)

quickSelect :: Ord a => Int -> [a] -> Maybe a
quickSelect n xs
  | n > length xs = Nothing
  | otherwise     = runST $ do
    arrST <- newArr (0, length xs - 1) xs
    let swap i j      = do
          vi <- readArray arrST i
          vj <- readArray arrST j
          writeArray arrST i vj
          writeArray arrST j vi
    let partition p q = do
          x <- readArray arrST p
          let loop i j
                | i > j     = swap p j $> j
                | otherwise = do
                  u <- readArray arrST i
                  if u < x then loop (i + 1) j else swap i j >> loop i (j - 1)
          loop (p + 1) q
    let qSelect i j
          | i >= j    = Just <$> readArray arrST i
          | otherwise = do
            gen <- unsafeIOToST getStdGen
            let (p, gen') = randomR (i, j) gen
            unsafeIOToST $ setStdGen gen'
            unless (i == p) $ swap i p
            k   <- partition i j
            if | k < n     -> qSelect (k + 1) j
               | k > n     -> qSelect i (k - 1)
               | otherwise -> Just <$> readArray arrST k
    qSelect 0 (length xs - 1)
  where
    newArr = newListArray :: (Int, Int) -> [a] -> ST s (STArray s Int a)

