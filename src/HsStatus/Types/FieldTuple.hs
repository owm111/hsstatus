{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module HsStatus.Types.FieldTuple
  ( FieldTuple (..)
  ) where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.IORef

import HsStatus.Types.Field
import HsStatus.Types.FieldValue

class FieldTuple a where
  type VarTuple a = vt | vt -> a
  type StateTuple a = st | st -> a
  startFields :: MVar () -> MVar () -> a -> IO (VarTuple a, IO ())
  readVars :: VarTuple a -> IO (StateTuple a)

instance (FieldValue a, FieldValue b) => FieldTuple (Field a, Field b) where
  type VarTuple (Field a, Field b) = (IORef a, IORef b)
  type StateTuple (Field a, Field b) = (a, b)
  startFields sem mvar (Field (af), Field (bf)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    pure ((av, bv), killThread at >> killThread bt)
  readVars (a, b) = (,) <$> readIORef a <*> readIORef b

instance (FieldValue a, FieldValue b, FieldValue c) =>  FieldTuple (Field a, Field b, Field c) where
  type VarTuple (Field a, Field b, Field c) = (IORef a, IORef b, IORef c)
  type StateTuple (Field a, Field b, Field c) = (a, b, c)
  startFields sem mvar (Field (af), Field (bf), Field (cf)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    cv <- newIORef initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    ct <- forkIO $ cf sem mvar cv
    pure ((av, bv, cv), killThread at >> killThread bt >> killThread ct)
  readVars (a, b, c) = (,,) <$> readIORef a <*> readIORef b <*> readIORef c

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d) => FieldTuple (Field a, Field b, Field c, Field d) where
  type VarTuple (Field a, Field b, Field c, Field d) = (IORef a, IORef b, IORef c, IORef d)
  type StateTuple (Field a, Field b, Field c, Field d) = (a, b, c, d)
  startFields sem mvar (Field (af), Field (bf), Field (cf), Field (df)) = do
    av <- newIORef initialValue
    bv <- newIORef initialValue
    cv <- newIORef initialValue
    dv <- newIORef initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    ct <- forkIO $ cf sem mvar cv
    dt <- forkIO $ df sem mvar dv
    pure ((av, bv, cv, dv), killThread at >> killThread bt >> killThread ct >> killThread dt)
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
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    ct <- forkIO $ cf sem mvar cv
    dt <- forkIO $ df sem mvar dv
    et <- forkIO $ ef sem mvar ev
    pure ((av, bv, cv, dv, ev), killThread at >> killThread bt >> killThread ct >> killThread dt >> killThread et)
  readVars (a, b, c, d, e) = (,,,,) <$> readIORef a <*> readIORef b <*> readIORef c <*> readIORef d <*> readIORef e
