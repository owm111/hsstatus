{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies #-}

module HsStatus.Types
  ( Field (..)
  , IOString
  , FormatterFor
  -- * 'Sem'
  , Sem
  , newSem
  , waitFor
  , stopWaitingFor
  -- * 'FieldTuple'
  , FieldTuple (..)
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString) 
import Data.Tuple.Update

-- | A field represents something that needs to be monitored and displayed. For
-- example, time, battery, volume, etc.
newtype Field a = Field ((Either IOString a -> IO ()) -> IO ())

-- | Alias for string type used for input and output.
type IOString = ByteString

-- | Alias for a function that converts @a@ to 'IOString'.
type FormatterFor a = a -> IOString

-- | Abstracted boolean semaphore.
type Sem = TMVar ()

-- | Creates a new 'Sem'.
newSem :: IO Sem
newSem = newEmptyTMVarIO

-- | Blocks until provided 'Sem' is signalled.
waitFor :: Sem -> IO ()
waitFor = atomically . takeTMVar

-- | Stops blocking 'waitFor' threads.
stopWaitingFor :: Sem -> IO ()
stopWaitingFor = atomically . flip putTMVar ()

class FieldTuple t where
  type StatesOf t
  startFields :: TQueue (StatesOf t -> StatesOf t) -> t -> [IO ()]
  initializeStatesOf :: t -> StatesOf t

instance FieldTuple (Field a, Field b) where
  type StatesOf (Field a, Field b) = (Either IOString a, Either IOString b)
  startFields queue (Field f1, Field f2) =
    [ f1 $ atomically . writeTQueue queue . upd1
    , f2 $ atomically . writeTQueue queue . upd2
    ]
  initializeStatesOf _ = (Left "Updating...", Left "Updating...")

instance FieldTuple (Field a, Field b, Field c) where
  type StatesOf (Field a, Field b, Field c) = (Either IOString a, Either IOString b, Either IOString c)
  startFields queue (Field f1, Field f2, Field f3) =
    [ f1 $ atomically . writeTQueue queue . upd1
    , f2 $ atomically . writeTQueue queue . upd2
    , f3 $ atomically . writeTQueue queue . upd3
    ]
  initializeStatesOf _ = (Left "Updating...", Left "Updating...", Left "Updating...")

instance FieldTuple (Field a, Field b, Field c, Field d) where
  type StatesOf (Field a, Field b, Field c, Field d) = (Either IOString a, Either IOString b, Either IOString c, Either IOString d)
  startFields queue (Field f1, Field f2, Field f3, Field f4) =
    [ f1 $ atomically . writeTQueue queue . upd1
    , f2 $ atomically . writeTQueue queue . upd2
    , f3 $ atomically . writeTQueue queue . upd3
    , f4 $ atomically . writeTQueue queue . upd4
    ]
  initializeStatesOf _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")

instance FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type StatesOf (Field a, Field b, Field c, Field d, Field e) = (Either IOString a, Either IOString b, Either IOString c, Either IOString d, Either IOString e)
  startFields queue (Field f1, Field f2, Field f3, Field f4, Field f5) =
    [ f1 $ atomically . writeTQueue queue . upd1
    , f2 $ atomically . writeTQueue queue . upd2
    , f3 $ atomically . writeTQueue queue . upd3
    , f4 $ atomically . writeTQueue queue . upd4
    , f5 $ atomically . writeTQueue queue . upd5
    ]
  initializeStatesOf _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")
