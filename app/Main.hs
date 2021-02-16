module Main where

import Data.ByteString (pack, unpack)
import Data.Char (isSpace)
import Lib
import GuessXOR (bestFor)
import Strings

main :: IO ()
main = do
    h <- getContents
    -- _ includes \n
    let (arg, _) = break isSpace h
    let buf = fromHex arg
    let hex = bestFor buf
    print $ toHex $ pack [hex]
    print $ pack $ xor (repeat hex) (unpack buf)
