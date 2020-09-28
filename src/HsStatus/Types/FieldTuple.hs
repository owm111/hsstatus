{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module HsStatus.Types.FieldTuple
  ( FieldTuple (..)
  ) where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.Vector (Vector)

import qualified Data.Vector as V

import HsStatus.Types.Field
import HsStatus.Types.FieldValue

class FieldTuple a where
  startFields :: Chan (Int, ByteString) -> MVar () -> a -> IO (Vector ByteString, IO ())

instance (FieldValue a, FieldValue b) => FieldTuple (Field a, Field b) where
  startFields chan mvar (Field (af), Field (bf)) = do
    at <- forkIO $ af 0 mvar chan
    bt <- forkIO $ bf 1 mvar chan
    pure (V.replicate 2 mempty, killThread at >> killThread bt)
  {-# INLINE startFields #-}

instance (FieldValue a, FieldValue b, FieldValue c) =>  FieldTuple (Field a, Field b, Field c) where
  startFields chan mvar (Field (af), Field (bf), Field (cf)) = do
    at <- forkIO $ af 0 mvar chan
    bt <- forkIO $ bf 1 mvar chan
    ct <- forkIO $ cf 2 mvar chan
    pure (V.replicate 3 mempty, killThread at >> killThread bt >> killThread ct)
  {-# INLINE startFields #-}

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d) => FieldTuple (Field a, Field b, Field c, Field d) where
  startFields chan mvar (Field (af), Field (bf), Field (cf), Field (df)) = do
    at <- forkIO $ af 0 mvar chan
    bt <- forkIO $ bf 1 mvar chan
    ct <- forkIO $ cf 2 mvar chan
    dt <- forkIO $ df 3 mvar chan
    pure (V.replicate 4 mempty, killThread at >> killThread bt >> killThread ct >> killThread dt)
  {-# INLINE startFields #-}

instance (FieldValue a, FieldValue b, FieldValue c, FieldValue d, FieldValue e) => FieldTuple (Field a, Field b, Field c, Field d, Field e) where
  startFields chan mvar (Field (af), Field (bf), Field (cf), Field (df), Field (ef)) = do
    at <- forkIO $ af 0 mvar chan
    bt <- forkIO $ bf 1 mvar chan
    ct <- forkIO $ cf 2 mvar chan
    dt <- forkIO $ df 3 mvar chan
    et <- forkIO $ ef 4 mvar chan
    pure (V.replicate 5 mempty, killThread at >> killThread bt >> killThread ct >> killThread dt >> killThread et)
  {-# INLINE startFields #-}
