{-# LANGUAGE FlexibleInstances #-}

module HsStatus.Types.FieldValue where

import Data.ByteString (ByteString)

class FieldValue a where
  initialValue :: a

instance FieldValue Int where
  initialValue = 0
  {-# INLINE initialValue #-}
instance FieldValue [a] where
  initialValue = []
  {-# INLINE initialValue #-}
instance FieldValue Bool where
  initialValue = False
  {-# INLINE initialValue #-}
instance FieldValue ByteString where
  initialValue = mempty
  {-# INLINE initialValue #-}
instance (FieldValue a, FieldValue b) => FieldValue (a,b) where
  initialValue = (initialValue, initialValue)
  {-# INLINE initialValue #-}
