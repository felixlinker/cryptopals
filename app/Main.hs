module Main where

import Data.Char (isSpace)
import Base64
import Strings

main :: IO ()
main = do
    h <- getContents
    print $ (b64Enc . fromHex) (filter (not . isSpace) h)
    return ()
