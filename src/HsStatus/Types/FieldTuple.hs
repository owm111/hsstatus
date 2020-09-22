{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}

module HsStatus.Types.FieldTuple
  ( FieldTuple (..)
  ) where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.IORef

import HsStatus.Types.Field
import HsStatus.Types.FieldValue

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
x <++> y = (++) <$> x <*> y
infixr 5 <++>

class FieldTuple a where
  type VarTuple a = vt | vt -> a
  type StateTuple a = st | st -> a
  startFields :: MVar () -> MVar () -> a -> IO (VarTuple a, [ThreadId])
  readVars :: VarTuple a -> IO (StateTuple a)

instance (FieldValue a, FieldValue b) => FieldTuple (Field a, Field b) where
  type VarTuple (Field a, Field b) = (IORef a, IORef b)
  type StateTuple (Field a, Field b) = (a, b)
  startFields sem mvar (Field (af), Field (bf)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    ((av, bv),) <$> af sem mvar av <++> bf sem mvar bv
  readVars (a, b) = (,) <$> readIORef a <*> readIORef b

instance (FieldValue a, FieldValue b, FieldValue c) =>  FieldTuple (Field a, Field b, Field c) where
  type VarTuple (Field a, Field b, Field c) = (IORef a, IORef b, IORef c)
  type StateTuple (Field a, Field b, Field c) = (a, b, c)
  startFields sem mvar (Field (af), Field (bf), Field (cf)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    cv <- newIORef initialValue
    ((av, bv, cv),) <$> af sem mvar av <++> bf sem mvar bv <++> cf sem mvar cv
  readVars (a, b, c) = (,,) <$> readIORef a <*> readIORef b <*> readIORef c

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d) => FieldTuple (Field a, Field b, Field c, Field d) where
  type VarTuple (Field a, Field b, Field c, Field d) = (IORef a, IORef b, IORef c, IORef d)
  type StateTuple (Field a, Field b, Field c, Field d) = (a, b, c, d)
  startFields sem mvar (Field (af), Field (bf), Field (cf), Field (df)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    cv <- newIORef initialValue
    dv <- newIORef initialValue
    ((av, bv, cv, dv),) <$> af sem mvar av <++> bf sem mvar bv <++> cf sem mvar cv <++> df sem mvar dv
  readVars (a, b, c, d) = (,,,) <$> readIORef a <*> readIORef b <*> readIORef c <*> readIORef d

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d, FieldValue e) => FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type VarTuple (Field a, Field b, Field c, Field d, Field e) = (IORef a, IORef b, IORef c, IORef d, IORef e)
  type StateTuple (Field a, Field b, Field c, Field d, Field e) = (a, b, c, d, e)
  startFields sem mvar (Field (af), Field (bf), Field (cf), Field (df), Field (ef)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    cv <- newIORef initialValue
    dv <- newIORef initialValue
    ev <- newIORef initialValue
    ((av, bv, cv, dv, ev),) <$> af sem mvar av <++> bf sem mvar bv <++> cf sem mvar cv <++> df sem mvar dv <++> ef sem mvar ev
  readVars (a, b, c, d, e) = (,,,,) <$> readIORef a <*> readIORef b <*> readIORef c <*> readIORef d <*> readIORef e
