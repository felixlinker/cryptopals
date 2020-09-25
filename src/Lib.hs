module Lib
    ( modExp
    , dh
    ) where

import qualified Data.Bits as B

(%) :: (B.Bits a, Integral a) => a -> a -> a
(%) = mod

modExp :: (B.Bits a, Integral a) => a -> a -> a -> a
modExp base exp p =
    let modExp' :: (B.Bits a, Integral a) => a -> a -> a -> a -> a
        modExp' acc base exp p
            | exp <= 0 = acc % p
            | otherwise =
                let acc'    = if exp % 2 == 1 then (acc * base) % p else acc
                    base'   = (base * base) % p
                    exp'    = B.shiftR exp 1
                in  modExp' acc' base' exp' p
    in modExp' 1 (base % p) exp p

dh :: (B.Bits a, Integral a) => a -> a -> a -> a -> (a, a, a)
dh a b generator p =
    let pkA = modExp generator a p
        pkB = modExp generator b p
    in  (pkA, pkB, modExp pkB a p)
