module HsStatus.IO
  ( module Data.ByteString
  , module Data.ByteString.Char8
  , module System.IO
  ) where

-- Re-export IO functions, replacing String-based ones with ByteString-based
-- ones. String-based functions with not ByteString equivalent are re-exported.

import System.IO hiding
  ( readFile, writeFile, appendFile
  , hGetContents, hGetLine
  , hPutStr, hPutStrLn --, hPrint
  , interact
  , putStr, putStrLn --, print
  , getLine, getContents
  --, readIO, readLn
  --, openTempFile, openBinaryTempFile, openTempFileWithDefaultPermissions
  --, openBinaryTempFileWithDefaultPermissions
  )

import Data.ByteString
 ( getLine, getContents
 , putStr
 , interact
 , readFile, writeFile, appendFile
 , hGetLine, hGetContents, hGetSome --, hGet, hGetNonBlocking
 , hPutStr --, hPut, hPutNonBlocking
 )

import Data.ByteString.Char8 (hPutStrLn, putStrLn)
