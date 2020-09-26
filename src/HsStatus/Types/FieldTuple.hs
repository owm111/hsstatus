{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
  type StateTuple a
  startFields :: TSem -> MVar () -> a -> IO (STM (StateTuple a), [ThreadId])

instance (FieldValue a, FieldValue b) => FieldTuple (Field a' a'' a, Field b' b'' b) where
  type StateTuple (Field a' a'' a, Field b' b'' b) = (a, b)
  startFields sem mvar (af, bf) = do
    (av, at) <- startField sem mvar af
    (bv, bt) <- startField sem mvar bf
    pure ((,) <$> readTVar av <*> readTVar bv, [at, bt])

instance (FieldValue a, FieldValue b, FieldValue c) =>  FieldTuple (Field a' a'' a, Field b' b'' b, Field c' c'' c) where
  type StateTuple (Field a' a'' a, Field b' b'' b, Field c' c'' c) = (a, b, c)
  startFields sem mvar (af, bf, cf) = do
    (av, at) <- startField sem mvar af
    (bv, bt) <- startField sem mvar bf
    (cv, ct) <- startField sem mvar cf
    pure ((,,) <$> readTVar av <*> readTVar bv <*> readTVar cv, [at, bt, ct])

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d) => FieldTuple (Field a' a'' a, Field b' b'' b, Field c' c'' c, Field d' d'' d) where
  type StateTuple (Field a' a'' a, Field b' b'' b, Field c' c'' c, Field d' d'' d) = (a, b, c, d)
  startFields sem mvar (af, bf, cf, df) = do
    (av, at) <- startField sem mvar af
    (bv, bt) <- startField sem mvar bf
    (cv, ct) <- startField sem mvar cf
    (dv, dt) <- startField sem mvar df
    pure ((,,,) <$> readTVar av <*> readTVar bv <*> readTVar cv <*> readTVar dv, [at, bt, ct, dt])

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d, FieldValue e) => FieldTuple (Field a' a'' a, Field b' b'' b, Field c' c'' c, Field d' d'' d, Field e' e'' e) where
  type StateTuple (Field a' a'' a, Field b' b'' b, Field c' c'' c, Field d' d'' d, Field e' e'' e) = (a, b, c, d, e)
  startFields sem mvar (af, bf, cf, df, ef) = do
    (av, at) <- startField sem mvar af
    (bv, bt) <- startField sem mvar bf
    (cv, ct) <- startField sem mvar cf
    (dv, dt) <- startField sem mvar df
    (ev, et) <- startField sem mvar ef
    pure ((,,,,) <$> readTVar av <*> readTVar bv <*> readTVar cv <*> readTVar dv <*> readTVar ev, [at, bt, ct, dt, et])
