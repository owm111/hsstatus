{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}

module HsStatus.Types.FieldTuple
  ( FieldTuple (..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.ByteString (ByteString)

import HsStatus.Types.Field

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
x <++> y = (++) <$> x <*> y
infixr 5 <++>

class FieldTuple a where
  type VarTuple a = vt | vt -> a
  type StateTuple a = st | st -> a
  startFields :: TSem -> a -> IO (VarTuple a, [ThreadId])
  readVars :: VarTuple a -> STM (StateTuple a)

instance FieldTuple (Field a, Field b) where
  type VarTuple (Field a, Field b) = (TVar a, TVar b)
  type StateTuple (Field a, Field b) = (a, b)
  startFields sem (Field (ai, af), Field (bi, bf)) = do
    av <- newTVarIO ai
    bv <- newTVarIO bi
    ((av, bv),) <$> af sem av <++> bf sem bv
  readVars (a, b) = (,) <$> readTVar a <*> readTVar b

instance FieldTuple (Field a, Field b, Field c) where
  type VarTuple (Field a, Field b, Field c) = (TVar a, TVar b, TVar c)
  type StateTuple (Field a, Field b, Field c) = (a, b, c)
  startFields sem (Field (ai, af), Field (bi, bf), Field (ci, cf)) = do
    av <- newTVarIO ai
    bv <- newTVarIO bi
    cv <- newTVarIO ci
    ((av, bv, cv),) <$> af sem av <++> bf sem bv <++> cf sem cv
  readVars (a, b, c) = (,,) <$> readTVar a <*> readTVar b <*> readTVar c

instance FieldTuple (Field a, Field b, Field c, Field d) where
  type VarTuple (Field a, Field b, Field c, Field d) = (TVar a, TVar b, TVar c, TVar d)
  type StateTuple (Field a, Field b, Field c, Field d) = (a, b, c, d)
  startFields sem (Field (ai, af), Field (bi, bf), Field (ci, cf), Field (di, df)) = do
    av <- newTVarIO ai
    bv <- newTVarIO bi
    cv <- newTVarIO ci
    dv <- newTVarIO di
    ((av, bv, cv, dv),) <$> af sem av <++> bf sem bv <++> cf sem cv <++> df sem dv
  readVars (a, b, c, d) = (,,,) <$> readTVar a <*> readTVar b <*> readTVar c <*> readTVar d

instance FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type VarTuple (Field a, Field b, Field c, Field d, Field e) = (TVar a, TVar b, TVar c, TVar d, TVar e)
  type StateTuple (Field a, Field b, Field c, Field d, Field e) = (a, b, c, d, e)
  startFields sem (Field (ai, af), Field (bi, bf), Field (ci, cf), Field (di, df), Field (ei, ef)) = do
    av <- newTVarIO ai
    bv <- newTVarIO bi
    cv <- newTVarIO ci
    dv <- newTVarIO di
    ev <- newTVarIO ei
    ((av, bv, cv, dv, ev),) <$> af sem av <++> bf sem bv <++> cf sem cv <++> df sem dv <++> ef sem ev
  readVars (a, b, c, d, e) = (,,,,) <$> readTVar a <*> readTVar b <*> readTVar c <*> readTVar d <*> readTVar e
