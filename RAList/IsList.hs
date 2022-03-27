{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RAList.IsList where

import           RAList.Internal (RAList(..))
import qualified RAList.Internal as R
import           GHC.Exts (IsList(..))

instance IsList (RAList e) where
  type Item (RAList e) = e
  fromList = foldr (:<) Empty
  toList   = R.toList
