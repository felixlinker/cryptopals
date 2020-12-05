module Main where

import Data.ByteString (pack, unpack)
import Data.Char (isSpace)
import Lib
import Strings

main :: IO ()
main = do
    h <- getContents
    -- sndArg includes \n
    let (fstArg, sndArg) = break isSpace h
    let fstBuf = unpack $ fromHex fstArg
    let sndBuf = unpack $ fromHex $ filter (not . isSpace) sndArg
    print $ toHex $ pack $ xor fstBuf sndBuf
    return ()
