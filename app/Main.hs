module Main where

import Data.Char (isSpace)
import Lib
import Numeric
import System.Environment
import qualified System.Random as R

genI :: Integer -> IO Integer
genI upperBound = R.randomRIO (0 :: Integer, upperBound)

main :: IO ()
main = do
    gS : _ <- getArgs
    let g = fst $ head $ readHex gS
    pS <- getContents
    let p = fst $ head $ readHex $ filter (not . isSpace) pS
    a <- genI p
    b <- genI p
    let (pkA, pkB, sk) = dh a b g p
    putStrLn $ showHex sk ""
    return ()
