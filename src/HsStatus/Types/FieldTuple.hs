{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HsStatus.Types.FieldTuple
  ( FieldTuple (..)
  ) where

import Data.ByteString (ByteString)
import Data.Tuple.Update

import HsStatus.Types.Field (Field)
import HsStatus.Types.Starter (Starter, fieldStarter)

class FieldTuple a where
  type StateTuple a 
  initialStateFor :: a -> StateTuple a
  fieldsStarter :: (forall x . (x -> StateTuple a -> StateTuple a) -> x -> IO ()) -> a -> Starter

instance FieldTuple (Field a, Field b) where
  type StateTuple (Field a, Field b) = (Either ByteString a, Either ByteString b)
  initialStateFor _ = (Left "Updating...", Left "Updating...")
  fieldsStarter send (f1,f2) =  fieldStarter (send upd1) f1
                             <> fieldStarter (send upd2) f2

instance FieldTuple (Field a, Field b, Field c) where
  type StateTuple (Field a, Field b, Field c) = (Either ByteString a, Either ByteString b, Either ByteString c)
  initialStateFor _ = (Left "Updating...", Left "Updating...", Left "Updating...")
  fieldsStarter send (f1,f2,f3) = fieldStarter (send upd1) f1 <> fieldStarter (send upd2) f2 <> fieldStarter (send upd3) f3

instance FieldTuple (Field a, Field b, Field c, Field d) where
  type StateTuple (Field a, Field b, Field c, Field d) = (Either ByteString a, Either ByteString b, Either ByteString c, Either ByteString d)
  initialStateFor _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")
  fieldsStarter send (f1,f2,f3,f4) = fieldStarter (send upd1) f1 <> fieldStarter (send upd2) f2 <> fieldStarter (send upd3) f3 <> fieldStarter (send upd4) f4

instance FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type StateTuple (Field a, Field b, Field c, Field d, Field e) = (Either ByteString a, Either ByteString b, Either ByteString c, Either ByteString d, Either ByteString e)
  initialStateFor _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")
  fieldsStarter send (f1,f2,f3,f4,f5) = fieldStarter (send upd1) f1 <> fieldStarter (send upd2) f2 <> fieldStarter (send upd3) f3 <> fieldStarter (send upd4) f4 <> fieldStarter (send upd5) f5
