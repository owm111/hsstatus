module HsStatus.Types.Watcher
  ( Watcher (..)
  , fromWatcher
  ) where

import Data.ByteString (ByteString)
import System.INotify (Event, EventVariety, INotify, WatchDescriptor, addWatch)

data Watcher = Watcher [EventVariety] ByteString

fromWatcher :: (Event -> IO ()) -> Watcher -> INotify -> IO [WatchDescriptor]
fromWatcher f (Watcher events path) = \inot -> fmap (:[]) (addWatch inot events path f)
