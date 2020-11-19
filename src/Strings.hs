module Strings
    ( fromHex
    , toHex )
    where

import Numeric ( readHex, showHex )
import Data.ByteString (ByteString, unpack, pack)
import Data.Word (Word8)

-- | Parses a string as hex ignoring case.
fromHex :: String -> ByteString
fromHex = pack . destruct
    where
        toWord8 :: String -> Word8
        toWord8 [] = 0
        toWord8 [h] = fst . head $ readHex [h, '0']
        toWord8 [h0, h1] = fst . head $ readHex [h0, h1]
        toWord8 _ = error "Can only convert strings up to length two"
        destruct :: String -> [Word8]
        destruct [] = []
        destruct s =
            let (h, t) = splitAt 2 s
            in toWord8 h : destruct t

-- | Outputs a hex-string.
toHex :: ByteString -> String
toHex s = foldr ((.) . showHex) id (unpack s) ""
