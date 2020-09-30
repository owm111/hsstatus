module HsStatus.Fields.Date
  ( dateField
  ) where

import Control.Concurrent
import Foreign
import Foreign.C
import HsStatus.Types.Field (Field (..))
import Streamly

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Array as A

dateField :: Int -> String -> Field
dateField delay format =
  S.bracket (allocPtrs format) freePtrs $ \(fmtStr,outStr,tmtPtr,stmPtr) -> S.repeatM$ do
    threadDelay delay
    c_time tmtPtr
    c_localtime_r tmtPtr stmPtr
    c_strftime outStr 128 fmtStr stmPtr
    A.fromList <$> peekCString outStr


{-# INLINE dateField #-}

type Ptrs = (CString, CString, Ptr CTime, Ptr CTm)

allocPtrs :: String -> IO Ptrs
allocPtrs format =
  (,,,) <$> newCString format <*> mallocArray0 128 <*> malloc <*> malloc

freePtrs :: Ptrs -> IO ()  
freePtrs (w,x,y,z) = free w >> free x >> free y >> free z

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
