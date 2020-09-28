module HsStatus.Fields.Date
  ( dateField
  ) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign
import Foreign.C

import HsStatus.Types.Field (Field (..))

dateField :: Int -> String -> Field String
dateField delay format = Field $ \printSem _ var ->
  withCString format $ \fmt ->
    alloca $ \timePtr ->
      allocaArray0 128 $ \strPtr ->
        allocaBytesAligned 56 8 $ \tmPtr ->
          forever $ do
            c_time timePtr
            c_localtime_r timePtr tmPtr
            c_strftime strPtr 128 fmt tmPtr
            writeIORef var =<< peekCString strPtr
            tryPutMVar printSem ()
            threadDelay delay

{-# INLINE dateField #-}

newtype CTm = CTm (Ptr CTm)

instance Storable CTm where
  sizeOf _ = 56
  alignment _ = 8
  peek = undefined
  poke = undefined

foreign import ccall unsafe "time.h time"
  c_time :: Ptr CTime -> IO CTime

foreign import ccall unsafe "time.h localtime_r"
  c_localtime_r :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

foreign import ccall unsafe "time.h strftime"
  c_strftime :: CString -> CSize -> CString -> Ptr CTm -> IO CSize
