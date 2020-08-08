{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies, ExistentialQuantification #-}

module HsStatus.Types
  ( Field (..)
  , Starter (..)
  , fieldStarter
  , IOString
  , FormatterFor
  , Watcher (..)
  , fromWatcher
  -- * 'Sem'
  , Sem
  , newSem
  , waitFor
  , stopWaitingFor
  -- * 'FieldTuple'
  , FieldTuple (..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString) 
import Data.Foldable
import Data.Tuple.Update
import System.INotify (INotify, WatchDescriptor, Event, EventVariety, addWatch)
import System.Process.Typed

-- | A field represents something that needs to be monitored and displayed. For
-- example, time, battery, volume, etc.
data Field a
  = RawField ((Either IOString a -> IO ()) -> IO ()) ((Either IOString a -> IO ()) -> IO ()) (Event -> IO (Either IOString a)) [Watcher]
  | SimpleField ((Either IOString a -> IO ()) -> IO ())
  | WatcherField [Watcher] (Maybe Event -> IO (Either IOString a))
  | JustWatcherField [Watcher] a (Event -> IO (Either IOString a))
  | PeriodicField Int (IO (Either IOString a))
  | forall i o e . ProcessField (ProcessConfig i o e) ((Either IOString a -> IO ()) -> Process i o e -> IO ())

data Starter = Starter
  { startup :: IO ()
  , threadsToFork :: [IO ()]
  , maybeAddWatchers :: Maybe (INotify -> IO [WatchDescriptor])
  }

instance Semigroup Starter where
  (Starter s1 t1 w1) <> (Starter s2 t2 w2) = Starter (s1 <> s2) (t1 <> t2) (w1 <> w2)
instance Monoid Starter where
  mempty = Starter mempty mempty mempty

fieldStarter :: (Either IOString a -> IO ()) -> Field a -> Starter
fieldStarter send (RawField t s wa ws) = Starter (s send) [t send] (Just $ foldMap (fromWatcher (send <=< wa)) ws)
fieldStarter send (SimpleField t) = mempty { threadsToFork = [t send] }
fieldStarter send (WatcherField ws a) = mempty { startup = send =<< a Nothing, maybeAddWatchers = Just $ foldMap (fromWatcher (send <=< a . Just)) ws }
fieldStarter send (JustWatcherField ws init a) = mempty { startup = send (Right init), maybeAddWatchers = Just $ foldMap (fromWatcher (send <=< a)) ws }
fieldStarter send (PeriodicField interval a) = mempty { threadsToFork = [ forever (a >>= send >> threadDelay interval) ] }
fieldStarter send (ProcessField conf go) = mempty { threadsToFork = [ withProcessWait conf (go send) ] }

data Watcher = Watcher [EventVariety] ByteString

fromWatcher :: (Event -> IO ()) -> Watcher -> INotify -> IO [WatchDescriptor]
fromWatcher f (Watcher events path) = \inot -> fmap (:[]) (addWatch inot events path f)

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

class FieldTuple a where
  type StateTuple a 
  initialStateFor :: a -> StateTuple a
  fieldsStarter :: TQueue (StateTuple a -> StateTuple a) -> a -> Starter

instance FieldTuple (Field a, Field b) where
  type StateTuple (Field a, Field b) = (Either IOString a, Either IOString b)
  initialStateFor _ = (Left "Updating...", Left "Updating...")
  fieldsStarter queue (f1,f2) =  fieldStarter (atomically . writeTQueue queue . upd1) f1
                              <> fieldStarter (atomically . writeTQueue queue . upd2) f2

instance FieldTuple (Field a, Field b, Field c) where
  type StateTuple (Field a, Field b, Field c) = (Either IOString a, Either IOString b, Either IOString c)
  initialStateFor _ = (Left "Updating...", Left "Updating...", Left "Updating...")
  fieldsStarter queue (f1,f2,f3) = fieldStarter (atomically . writeTQueue queue . upd1) f1 <> fieldStarter (atomically . writeTQueue queue . upd2) f2 <> fieldStarter (atomically . writeTQueue queue . upd3) f3

instance FieldTuple (Field a, Field b, Field c, Field d) where
  type StateTuple (Field a, Field b, Field c, Field d) = (Either IOString a, Either IOString b, Either IOString c, Either IOString d)
  initialStateFor _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")
  fieldsStarter queue (f1,f2,f3,f4) = fieldStarter (atomically . writeTQueue queue . upd1) f1 <> fieldStarter (atomically . writeTQueue queue . upd2) f2 <> fieldStarter (atomically . writeTQueue queue . upd3) f3 <> fieldStarter (atomically . writeTQueue queue . upd4) f4

instance FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type StateTuple (Field a, Field b, Field c, Field d, Field e) = (Either IOString a, Either IOString b, Either IOString c, Either IOString d, Either IOString e)
  initialStateFor _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")
  fieldsStarter queue (f1,f2,f3,f4,f5) = fieldStarter (atomically . writeTQueue queue . upd1) f1 <> fieldStarter (atomically . writeTQueue queue . upd2) f2 <> fieldStarter (atomically . writeTQueue queue . upd3) f3 <> fieldStarter (atomically . writeTQueue queue . upd4) f4 <> fieldStarter (atomically . writeTQueue queue . upd5) f5
