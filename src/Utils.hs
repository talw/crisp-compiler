module Utils where

import Data.Word
import Data.List (elemIndex)

tr :: [Char] -> [Char] -> String -> String
tr as bs str = flip map str $
  \c -> maybe c (\ix -> bs !! ix) $ elemIndex c as

readBinary :: String -> Word32
readBinary = bin2dec . read

bin2dec :: Word32 -> Word32
bin2dec = convertBase 2 10

{-dec2bin :: Word32 -> Word32-}
{-dec2bin = convertBase 10 2-}

convertBase :: Integral a => a -> a -> a -> a
convertBase fromBase toBase = convertDec 10 toBase . convertDec fromBase 10
  where convertDec fb tb n = go n 1
          where go 0 _ = 0
                go x fac = if lsb `elem` [0..min fb tb - 1]
                             then addition + go (x `div` tb) (fac*fb)
                             else error "convertBase - invalid character"
                  where lsb = x `mod` tb
                        addition = lsb*fac
