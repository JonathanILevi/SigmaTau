{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ComponentID (ComponentID(..)) where

import Data.Hashable

newtype ComponentID = ComponentID Int
	deriving (Show,Eq,Ord,Hashable)





