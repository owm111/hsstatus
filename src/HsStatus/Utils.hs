{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Utils
  ( unzipWithM
  , seconds
  , second
  , percentTruncatedTo
  , readPercentTruncatedTo
  , dzenLength
  , setRightTo
  , readIntEither
  , packExceptions
  , hGetLineEither
  , hSeekEither
  , hGetFirstLine
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Zip
import Data.Bifunctor hiding (second)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, readInt)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Function
import Data.Functor
import System.IO.Error

import HsStatus.Types
import HsStatus.IO

-- | @unzipWith@ lifted to a monad, i.e., a combination of 'munzip' and 'mapM'.
-- Given a traversable and a function that returns a tuple, return a pair of
-- traversables within a monad.
unzipWithM :: (Monad m, Traversable t, MonadZip t) => (a -> m (b, c)) -> t a -> m (t b, t c)
unzipWithM f = fmap munzip . mapM f

-- | Milliseconds to seconds.
seconds :: Int -> Int
seconds = (* 1000000)

-- | One second.
second :: Int
second = seconds 1

-- | Returns a percent truncated to a certain number digits.
--
-- TODO: Int returning variant.
percentTruncatedTo :: Int -> Int -> Int -> Double
percentTruncatedTo digits num den
  | 0 <- digits = nOverDTimes 100
  | 1 <- digits = nOverDTimes 1000  / 10
  | 2 <- digits = nOverDTimes 10000 / 100
  | otherwise   = (nOverDTimes $ 10^(digits + 2)) / (10^digits)
  where nOverDTimes = fromIntOverDen . (num *)
        fromIntOverDen = fromIntegral . (`div` den)

readPercentTruncatedTo :: Int -> ByteString -> ByteString -> Double
readPercentTruncatedTo digits = percentTruncatedTo digits `on` read'
  where read' str = case readInt str of Just (i, _) -> i

dzenLength :: ByteString -> Int
dzenLength s = dzenLength' s 0

dzenLength' :: ByteString -> Int -> Int
dzenLength' str n
  | BS.null str = n
  | BS.length afterCaret < 2 = BS.length str + n
  | first2After == "^^" = dzenLength' restAfter $! (n + lengthBefore + 1)
  | otherwise = dzenLength' (BS.tail restWithHead) $! (n + lengthBefore)
  where (beforeCaret, afterCaret) = BS.break (== caret) str
        (first2After, restAfter) = BS.splitAt 2 afterCaret
        caretIsEscaped = BS.take 2 afterCaret == "^^"
        lengthBefore = BS.length beforeCaret
        restWithHead = BS.dropWhile (/= closeParen) afterCaret

closeParen, caret :: Word8
closeParen = fromIntegral $ fromEnum ')'
caret = fromIntegral $ fromEnum '^'

setRightTo = (<$)

hSeekEither :: SeekMode -> Integer -> Either IOError Handle -> IO (Either IOError Handle)
hSeekEither m n (Right h) = tryIOError (hSeek h m n) <&> setRightTo h
hSeekEither _ _ e = return e

hGetLineEither :: Either IOError Handle -> IO (Either IOError ByteString)
hGetLineEither = either (return . Left) (try . hGetLine)

hGetFirstLine :: Either IOError Handle -> IO (Either IOError ByteString)
hGetFirstLine = hSeekEither AbsoluteSeek 0 >=> hGetLineEither

readIntEither :: Either IOError ByteString -> Either IOError Int
readIntEither = (>>= maybe (Left err) (Right . fst) . readInt)
  where err = userError "TODO: message"

packExceptions :: Exception e => Either e a -> Either ByteString a
packExceptions = first (pack . displayException)
