module HsStatus.Types.Starter
  ( Starter (..)
  , fieldStarter
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad ((<=<), forever)
import Data.ByteString (ByteString)
import System.INotify (INotify, WatchDescriptor)
import System.Process.Typed (withProcessWait)

import HsStatus.Types.Field (Field (..))
import HsStatus.Types.Watcher (fromWatcher)

data Starter = Starter
  { startup :: IO ()
  , threadsToFork :: [IO ()]
  , maybeAddWatchers :: Maybe (INotify -> IO [WatchDescriptor])
  }

instance Semigroup Starter where
  (Starter s1 t1 w1) <> (Starter s2 t2 w2) = Starter (s1 <> s2) (t1 <> t2) (w1 <> w2)
instance Monoid Starter where
  mempty = Starter mempty mempty mempty

fieldStarter :: (Either ByteString a -> IO ()) -> Field a -> Starter
fieldStarter send (RawField t s wa ws) = Starter (s send) [t send] (Just $ foldMap (fromWatcher (send <=< wa)) ws)
fieldStarter send (SimpleField t) = mempty { threadsToFork = [t send] }
fieldStarter send (WatcherField ws a) = mempty { startup = send =<< a Nothing, maybeAddWatchers = Just $ foldMap (fromWatcher (send <=< a . Just)) ws }
fieldStarter send (JustWatcherField ws init a) = mempty { startup = send (Right init), maybeAddWatchers = Just $ foldMap (fromWatcher (send <=< a)) ws }
fieldStarter send (PeriodicField interval a) = mempty { threadsToFork = [ forever (a >>= send >> threadDelay interval) ] }
fieldStarter send (ProcessField conf go) = mempty { threadsToFork = [ withProcessWait conf (go send) ] }
