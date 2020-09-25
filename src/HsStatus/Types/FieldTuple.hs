{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module HsStatus.Types.FieldTuple
  ( FieldTuple (..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.ByteString (ByteString)

import HsStatus.Types.Field
import HsStatus.Types.FieldValue

class FieldTuple a where
  type VarTuple a = vt | vt -> a
  type StateTuple a = st | st -> a
  startFields :: TSem -> MVar () -> a -> IO (VarTuple a, [ThreadId])
  readVars :: VarTuple a -> STM (StateTuple a)

instance (FieldValue a, FieldValue b) => FieldTuple (Field a, Field b) where
  type VarTuple (Field a, Field b) = (TVar a, TVar b)
  type StateTuple (Field a, Field b) = (a, b)
  startFields sem mvar (Field (af), Field (bf)) = do
    av <- newTVarIO initialValue
    bv <- newTVarIO initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    pure ((av, bv), [at, bt])
  readVars (a, b) = (,) <$> readTVar a <*> readTVar b

instance (FieldValue a, FieldValue b, FieldValue c) =>  FieldTuple (Field a, Field b, Field c) where
  type VarTuple (Field a, Field b, Field c) = (TVar a, TVar b, TVar c)
  type StateTuple (Field a, Field b, Field c) = (a, b, c)
  startFields sem mvar (Field (af), Field (bf), Field (cf)) = do
    av <- newTVarIO initialValue
    bv <- newTVarIO initialValue
    cv <- newTVarIO initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    ct <- forkIO $ cf sem mvar cv
    pure ((av, bv, cv), [at, bt, ct])
  readVars (a, b, c) = (,,) <$> readTVar a <*> readTVar b <*> readTVar c

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d) => FieldTuple (Field a, Field b, Field c, Field d) where
  type VarTuple (Field a, Field b, Field c, Field d) = (TVar a, TVar b, TVar c, TVar d)
  type StateTuple (Field a, Field b, Field c, Field d) = (a, b, c, d)
  startFields sem mvar (Field (af), Field (bf), Field (cf), Field (df)) = do
    av <- newTVarIO initialValue
    bv <- newTVarIO initialValue
    cv <- newTVarIO initialValue
    dv <- newTVarIO initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    ct <- forkIO $ cf sem mvar cv
    dt <- forkIO $ df sem mvar dv
    pure ((av, bv, cv, dv), [at, bt, ct, dt])
  readVars (a, b, c, d) = (,,,) <$> readTVar a <*> readTVar b <*> readTVar c <*> readTVar d

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d, FieldValue e) => FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type VarTuple (Field a, Field b, Field c, Field d, Field e) = (TVar a, TVar b, TVar c, TVar d, TVar e)
  type StateTuple (Field a, Field b, Field c, Field d, Field e) = (a, b, c, d, e)
  startFields sem mvar (Field (af), Field (bf), Field (cf), Field (df), Field (ef)) = do
    av <- newTVarIO initialValue
    bv <- newTVarIO initialValue
    cv <- newTVarIO initialValue
    dv <- newTVarIO initialValue
    ev <- newTVarIO initialValue
    at <- forkIO $ af sem mvar av
    bt <- forkIO $ bf sem mvar bv
    ct <- forkIO $ cf sem mvar cv
    dt <- forkIO $ df sem mvar dv
    et <- forkIO $ ef sem mvar ev
    pure ((av, bv, cv, dv, ev), [at, bt, ct, dt, et])
  readVars (a, b, c, d, e) = (,,,,) <$> readTVar a <*> readTVar b <*> readTVar c <*> readTVar d <*> readTVar e
