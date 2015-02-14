module Immediates where

import Data.Bits
import Data.Word
import Data.List (elemIndex)
import Data.Bool (bool)
import Debug.Trace (traceId, traceShowId)
import Data.Char (chr)

data ImmediateType
  = Fixnum
  | Char
  | Nil
  | Bool

maskOfFormat :: String -> Word32
maskOfFormat = readBinary . tr "0*" "10"

formatMasked :: String -> Word32
formatMasked  = readBinary . tr "*" "0"

shiftWidthOfFormat :: String -> Int
shiftWidthOfFormat = negate . length . takeWhile (/= '*') . reverse

fixnumFormat, charFormat, nilFormat, boolFormat :: String
fixnumFormat =       "00"
charFormat   = "00001111"
nilFormat    = "00111111"
boolFormat   = "0*101111"

true, false :: Word32
true = readBinary $ tr "*" "1" boolFormat
false = readBinary $ tr "*" "0" boolFormat

showImmediate :: Word32 -> String
showImmediate n =
  handleValueOfType [ (boolFormat, bool "#f" "#t" . (/= 0))
                    , (fixnumFormat, show)
                    , (charFormat, \n -> "#\\" ++ [chr $ fromIntegral n])
                    , (nilFormat, const "()")
                    ]
 where
  handleValueOfType :: [(String, Word32 -> String)] -> String
  handleValueOfType = foldr combineHandlers ""
   where
    applyMask mask = n .&. mask
    combineHandlers (format, handler) outputVal
      | outputVal /= "" = outputVal
      | applyMask mask == formatMasked format =
        let actualValue = shift (complement mask .&. n)
                                (shiftWidthOfFormat format)
        in handler actualValue
      | otherwise = ""
     where
      mask = maskOfFormat format




{-import Utils-}
tr :: [Char] -> [Char] -> String -> String
tr as bs str = flip map str $
  \c -> maybe c (\ix -> bs !! ix) $ elemIndex c as

readBinary :: String -> Word32
readBinary = bin2dec . read

bin2dec :: Word32 -> Word32
bin2dec = convertBase 2 10

convertBase :: Integral a => a -> a -> a -> a
convertBase fromBase toBase = convertDec 10 toBase . convertDec fromBase 10
  where convertDec fb tb n = go n 1
          where go 0 _ = 0
                go x fac = if lsb `elem` [0..min fb tb - 1]
                             then addition + go (x `div` tb) (fac*fb)
                             else error "convertBase - invalid character"
                  where lsb = x `mod` tb
                        addition = lsb*fac

{-boolMask,boolMasked :: Word32-}
{-boolMask = maskOfFormat boolFormat-}
{-boolMasked = readBinary $ tr "*" "0" boolFormat-}
{-fixnumMask,fixnumMasked :: Word32-}
{-fixnumMask = maskOfFormat fixnumFormat-}
{-fixnumMasked = readBinary $ tr "*" "0" fixnumFormat-}
