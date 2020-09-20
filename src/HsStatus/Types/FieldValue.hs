{-# LANGUAGE FlexibleInstances #-}

module HsStatus.Types.FieldValue where

import Data.ByteString (ByteString)

class FieldValue a where
  initialValue :: a

instance FieldValue Int        where initialValue = 0
instance FieldValue [a]        where initialValue = []
instance FieldValue Bool       where initialValue = False
instance FieldValue ByteString where initialValue = mempty

instance (FieldValue a, FieldValue b) => FieldValue (a,b) where
  initialValue = (initialValue, initialValue)
