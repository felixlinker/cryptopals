module Main where

import Lib
import qualified System.Random as R

genI :: Integer -> IO Integer
genI upperBound = R.randomRIO (0 :: Integer, upperBound)

main :: IO ()
main = return ()
