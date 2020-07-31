{-# LANGUAGE OverloadedStrings #-}

module HsStatus.Utils
  ( traverseAndUnzip
  , seconds
  , second
  , percentTruncatedTo
  , readPercentTruncatedTo
  , dzenLength
  ) where

import Data.ByteString.Char8 (readInt)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Function
import System.IO

import HsStatus.Types

-- | Given a traversable and a function that returns a tuple, return a pair of traversables.
traverseAndUnzip :: (Applicative m, Traversable t, Unzippable t) => (a -> m (b, c)) -> t a -> m (t b, t c)
traverseAndUnzip f xs = unzipF <$> traverse f xs

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

readPercentTruncatedTo :: Int -> IOString -> IOString -> Double
readPercentTruncatedTo digits = percentTruncatedTo digits `on` read'
  where read' str = case readInt str of Just (i, _) -> i

dzenLength :: IOString -> Int
dzenLength s = dzenLength' s 0

dzenLength' :: IOString -> Int -> Int
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
