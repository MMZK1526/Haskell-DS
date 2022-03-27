{-# LANGUAGE TypeFamilies #-}

module Gadgets.RAList.IsList where

import           Gadgets.RAList.Internal (RAList(..))
import qualified Gadgets.RAList.Internal as R
import           GHC.Exts (IsList(..))

instance IsList (RAList e) where
  type Item (RAList e) = e
  fromList = foldr (:<) Empty
  toList   = R.toList
