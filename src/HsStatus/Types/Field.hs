{-# LANGUAGE ExistentialQuantification #-}

module HsStatus.Types.Field
  ( Field (..)
  ) where

import Data.ByteString (ByteString)
import System.INotify (Event)
import System.Process.Typed (Process, ProcessConfig)

import HsStatus.Types.Watcher (Watcher)

data Field a
  = RawField ((Either ByteString a -> IO ()) -> IO ()) ((Either ByteString a -> IO ()) -> IO ()) (Event -> IO (Either ByteString a)) [Watcher]
  | SimpleField ((Either ByteString a -> IO ()) -> IO ())
  | WatcherField [Watcher] (Maybe Event -> IO (Either ByteString a))
  | JustWatcherField [Watcher] a (Event -> IO (Either ByteString a))
  | PeriodicField Int (IO (Either ByteString a))
  | forall i o e . ProcessField (ProcessConfig i o e) ((Either ByteString a -> IO ()) -> Process i o e -> IO ())
