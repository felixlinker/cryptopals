module GuessXOR where

import Data.Char (isPrint, toUpper)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)

type CharMap a = Map.Map Char a

opt :: [Char]
opt = "ETAOIN SHRDLU"

optCost :: [Char] -> Int
optCost = optCost' 0 (zip [0..] opt) . Map.fromList . (`zip` [0..])
    where
        optCost' :: Int -> [(Int, Char)] -> CharMap Int -> Int
        optCost' c [] mem =  c + Map.foldl (const succ) 0 mem * length opt
        optCost' c ((i, h):t) mem =
            let c' = c + maybe 0 (abs . (i -)) (Map.lookup h mem)
            in optCost' c' t $ Map.delete h mem

try :: BS.ByteString -> Word8 -> Int
try bs w =
    let plain = C8.map toUpper $ C8.filter isPrint $ BS.map (xor w) bs
        charMap = C8.foldr (Map.alter inc) Map.empty plain
        str = map fst $ sortOn snd $ Map.assocs charMap
    in BS.length bs - BS.length plain
    where
        inc :: Integral a => Maybe a -> Maybe a
        inc Nothing = Just 1
        inc (Just v) = Just $ succ v

type Scored = (Word8, Int)
bestFor :: BS.ByteString -> Word8
bestFor bs =
    let initialScored = ((,) <*> try bs) 0x00
    in tryRec bs [0x01..0xFF] initialScored
    where
        tryRec :: BS.ByteString -> [Word8] -> Scored -> Word8
        tryRec _ [] (w, _) = w
        tryRec _ _ (w, 0) = w
        tryRec bs (h:t) (w, score) =
            let score' = try bs h
                scored' = (if score' < score then h else w, min score' score)
            in tryRec bs t scored'
