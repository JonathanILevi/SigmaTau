{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ComponentID (ComponentID(..)) where

newtype ComponentID = ComponentID Int
	deriving (Show,Eq)





