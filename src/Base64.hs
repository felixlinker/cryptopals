{-# LANGUAGE OverloadedStrings #-}
module Base64
    ( b64EncList
    , b64Enc )
    where

import Data.Bits as B ((.|.), (.&.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.Word as W
import qualified Data.Map.Strict as M

-- | Maps bytes with two leading zeros to the respective base64 encoding.
encodingTable :: M.Map W.Word8 Char
encodingTable = M.fromDistinctAscList   [ (0, 'A'), (1, 'B'), (2, 'C'), (3, 'D')
                                        , (4, 'E'), (5, 'F'), (6, 'G'), (7, 'H')
                                        , (8, 'I'), (9, 'J'), (10, 'K')
                                        , (11, 'L'), (12, 'M'), (13, 'N')
                                        , (14, 'O'), (15, 'P'), (16, 'Q')
                                        , (17, 'R'), (18, 'S'), (19, 'T')
                                        , (20, 'U'), (21, 'V'), (22, 'W')
                                        , (23, 'X'), (24, 'Y'), (25, 'Z')
                                        , (26, 'a'), (27, 'b'), (28, 'c')
                                        , (29, 'd'), (30, 'e'), (31, 'f')
                                        , (32, 'g'), (33, 'h'), (34, 'i')
                                        , (35, 'j'), (36, 'k'), (37, 'l')
                                        , (38, 'm'), (39, 'n'), (40, 'o')
                                        , (41, 'p'), (42, 'q'), (43, 'r')
                                        , (44, 's'), (45, 't'), (46, 'u')
                                        , (47, 'v'), (48, 'w'), (49, 'x')
                                        , (50, 'y'), (51, 'z'), (52, '0')
                                        , (53, '1'), (54, '2'), (55, '3')
                                        , (56, '4'), (57, '5'), (58, '6')
                                        , (59, '7'), (60, '8'), (61, '9')
                                        , (62, '+'), (63, '/') ]

-- | Encode a byte in base64 ignoring first two leading bits.
encode :: W.Word8 -> Char
encode = (encodingTable M.!) . (63 .&.)

-- | Store the state of base64 encoding. Meant for consumption of one byte at a time.
data B64State = B64State    { lastByte :: W.Word8   -- ^ When iterating through
                                                    -- a sequence of bytes, only
                                                    -- 6 bits at a time can be
                                                    -- taken into account. These
                                                    -- are the left-over bits
                                                    -- from the previous iteration.
                            , offset :: Int -- ^ This is the index at which
                                            -- the bytes of the next iteration
                                            -- start.
                            } deriving Show

-- | Consume the next byte of the stream to encode and merge it with left over
-- bits from previous iteration.
pushByte :: B64State -> W.Word8 -> W.Word8
pushByte (B64State lb off) b = lb .|. shiftR b off

-- | Consume the next byte of the stream to encode and advance the encoding state.
incState :: B64State -> W.Word8 -> (W.Word8, B64State)
incState enc b =
    let off' = mod (offset enc + 2) 8
    in  (pushByte enc b, B64State (shiftL b $ mod (8 - off') 8) off')

-- | Encode base64 bytes. Leading two bits of encoded bytes are not necessarily
-- zero and must be ignored.
b64EncBytes :: [W.Word8] -> [W.Word8]
b64EncBytes = rec $ B64State 0 2
    where
        rec :: B64State -> [W.Word8] -> [W.Word8]
        rec enc [] =
            -- If offset is 2 at the end, we encoded a number of bytes divisble by 3
            if (offset enc == 2)
            then []
            -- Otherwise, pad the last bit in the encoder with a 0 byte
            else [pushByte enc 0]
        rec enc (b:bs) = if offset enc == 0
            then lastByte enc : rec (B64State 0 2) (b:bs)
            else let (encd, enc') = incState enc b in encd : rec enc' bs

-- | Encode as base64 string.
b64EncList :: [W.Word8] -> String
b64EncList = map encode . b64EncBytes

-- | Encode as base64 string.
b64Enc :: BS.ByteString -> String
b64Enc = b64EncList . BS.unpack
