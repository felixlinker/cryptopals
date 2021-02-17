module Main where

import Data.ByteString (pack, unpack)
import Data.Char (isSpace)
import Data.List (sortOn)
import Lib
import GuessXOR
import Strings ( fromHex, toHex )

main :: IO ()
main = do
    h <- getContents
    let args = lines h
    let (buf, Scored hex _) = head $ sortOn snd $ map (((,) <*> bestFor) . fromHex) args
    print $ toHex $ pack [hex]
    print $ pack $ xor (repeat hex) (unpack buf)
