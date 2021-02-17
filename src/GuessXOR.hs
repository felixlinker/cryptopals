module GuessXOR
    ( bestFor
    , Scored (..) ) where

import Data.Char (isPrint, toUpper)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.List (sortOn)

type RankMap = Map.Map Char Int

opt :: String
opt = "ETAOIN SHRDLU"

-- | Maps an upper-case character to its rank of its appearance frequency in the
-- english alphabet, e.g.
-- >>> Map.lookup 'E' optM
-- Just 0
--
-- Because E is the most frequent letter in the english alphabet.
optM :: RankMap
optM = Map.fromList $ zip opt [0..]

maxCost :: Int
maxCost = Map.size optM

-- | Determines a distance cost for 'optM' where 0 is the theoretical optimum
-- cost.
optCost :: RankMap -> Int
optCost = Map.foldl (+) 0 . Merge.merge notInEither notInEither mergeInBoth optM
    where
        notInEither :: Merge.SimpleWhenMissing Char Int Int
        notInEither = Merge.mapMissing (\_ _ -> maxCost)
        mergeInBoth :: Merge.SimpleWhenMatched Char Int Int Int
        mergeInBoth = Merge.zipWithMatched $ const (-)

-- | Calculate the 'optCost' for the input string XORed with the input byte.
try :: BS.ByteString -> Word8 -> Scored
try bs w =
    let plain = C8.map toUpper $ BS.map (xor w) bs
        -- Map of (char, quantity)
        charMap = C8.foldr (Map.alter inc) Map.empty plain
        ranks :: RankMap
        ranks = Map.fromList $ zipWith (\i t -> (fst t, i)) [0..]
                             $ sortOn snd
                             $ Map.toList charMap
    in Scored w $ optCost ranks
    where
        inc :: Integral a => Maybe a -> Maybe a
        inc = Just . maybe 1 succ

data Scored = Scored Word8 Int
instance Eq Scored where
    (==) (Scored _ scr) (Scored _ scr') = scr == scr'

instance Ord Scored where
    compare (Scored _ scr) (Scored _ scr') = compare scr scr'

-- | Guess a likely byte the input might have been XOR-obfuscated with.
bestFor :: BS.ByteString -> Scored
bestFor bs =
    let initialScored = try bs 0x00
    in tryRec bs [0x01..0xFF] initialScored
    where
        tryRec :: BS.ByteString -> [Word8] -> Scored -> Scored
        tryRec _ [] scrd = scrd
        tryRec _ _ (Scored w 0) = Scored w 0
        tryRec bs (h:t) score = tryRec bs t (min score $ try bs h)
