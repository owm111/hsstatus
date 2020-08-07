{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies #-}

module HsStatus.Types
  ( Field (..)
  , IOString
  , FormatterFor
  , Watcher (..)
  -- * 'Sem'
  , Sem
  , newSem
  , waitFor
  , stopWaitingFor
  -- * 'FieldTuple'
  , FieldTuple (..)
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString) 
import Data.Foldable
import Data.Tuple.Update
import qualified System.INotify as IN

-- | A field represents something that needs to be monitored and displayed. For
-- example, time, battery, volume, etc.
data Field a = Field
  { onFieldStart :: Maybe ((Either IOString a -> IO ()) -> IO ())
  , inotWatchers :: [Watcher a]
  }

data Watcher a = Watcher
  { watcherPath   :: ByteString
  , watcherEvents :: [IN.EventVariety]
  , watcherAction :: Maybe IN.Event -> IO (Either IOString a)
  }

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

watcherToFn :: (Either IOString a -> IO ()) -> Watcher a -> IN.INotify -> IO ()
watcherToFn send (Watcher path events action) inot = do
  IN.addWatch inot events path (send <=< action . Just)
  send =<< action Nothing

makeINotifyAction :: [IN.INotify -> IO ()] -> IO ()
makeINotifyAction watchers = do
  signal <- newSem
  IN.withINotify (fold (watchers ++ [const (waitFor signal)]))

instance FieldTuple (Field a, Field b) where
  type StatesOf (Field a, Field b) = (Either IOString a, Either IOString b)
  startFields queue (Field f1 w1, Field f2 w2) =
    let watchers =  map (watcherToFn (atomically . writeTQueue queue . upd1)) w1
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd2)) w2
        ai = if null watchers then [] else [makeINotifyAction watchers]
        a1 = case f1 of { Just x -> (x $ atomically . writeTQueue queue . upd1):ai ; _ -> ai }
        a2 = case f2 of { Just x -> (x $ atomically . writeTQueue queue . upd2):a1 ; _ -> a1 }
    in a2
  initializeStatesOf _ = (Left "Updating...", Left "Updating...")

instance FieldTuple (Field a, Field b, Field c) where
  type StatesOf (Field a, Field b, Field c) = (Either IOString a, Either IOString b, Either IOString c)
  startFields queue (Field f1 w1, Field f2 w2, Field f3 w3) =
    let watchers =  map (watcherToFn (atomically . writeTQueue queue . upd1)) w1
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd2)) w2
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd3)) w3
        ai = if null watchers then [] else [makeINotifyAction watchers]
        a1 = case f1 of { Just x -> (x $ atomically . writeTQueue queue . upd1):ai ; _ -> ai }
        a2 = case f2 of { Just x -> (x $ atomically . writeTQueue queue . upd2):a1 ; _ -> a1 }
        a3 = case f3 of { Just x -> (x $ atomically . writeTQueue queue . upd3):a2 ; _ -> a2 }
    in a3
  initializeStatesOf _ = (Left "Updating...", Left "Updating...", Left "Updating...")

instance FieldTuple (Field a, Field b, Field c, Field d) where
  type StatesOf (Field a, Field b, Field c, Field d) = (Either IOString a, Either IOString b, Either IOString c, Either IOString d)
  startFields queue (Field f1 w1, Field f2 w2, Field f3 w3, Field f4 w4) =
    let watchers =  map (watcherToFn (atomically . writeTQueue queue . upd1)) w1
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd2)) w2
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd3)) w3
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd4)) w4
        ai = if null watchers then [] else [makeINotifyAction watchers]
        a1 = case f1 of { Just x -> (x $ atomically . writeTQueue queue . upd1):ai ; _ -> ai }
        a2 = case f2 of { Just x -> (x $ atomically . writeTQueue queue . upd2):a1 ; _ -> a1 }
        a3 = case f3 of { Just x -> (x $ atomically . writeTQueue queue . upd3):a2 ; _ -> a2 }
        a4 = case f4 of { Just x -> (x $ atomically . writeTQueue queue . upd4):a3 ; _ -> a3 }
    in a4
  initializeStatesOf _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")

instance FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  type StatesOf (Field a, Field b, Field c, Field d, Field e) = (Either IOString a, Either IOString b, Either IOString c, Either IOString d, Either IOString e)
  startFields queue (Field f1 w1, Field f2 w2, Field f3 w3, Field f4 w4, Field f5 w5) =
    let watchers =  map (watcherToFn (atomically . writeTQueue queue . upd1)) w1
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd2)) w2
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd3)) w3
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd4)) w4
                 ++ map (watcherToFn (atomically . writeTQueue queue . upd5)) w5
        ai = if null watchers then [] else [makeINotifyAction watchers]
        a1 = case f1 of { Just x -> (x $ atomically . writeTQueue queue . upd1):ai ; _ -> ai }
        a2 = case f2 of { Just x -> (x $ atomically . writeTQueue queue . upd2):a1 ; _ -> a1 }
        a3 = case f3 of { Just x -> (x $ atomically . writeTQueue queue . upd3):a2 ; _ -> a2 }
        a4 = case f4 of { Just x -> (x $ atomically . writeTQueue queue . upd4):a3 ; _ -> a3 }
        a5 = case f5 of { Just x -> (x $ atomically . writeTQueue queue . upd5):a4 ; _ -> a4 }
    in a5
  initializeStatesOf _ = (Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...", Left "Updating...")
