module Immediates
  ( module Data.Bits
  , module Immediates
  )

where

import Data.Bits
import Data.Word
import Data.Bool (bool)
import Data.Char (chr, ord)

import Utils
import Text.Printf (printf)

maskOfFormat :: String -> Word32
maskOfFormat = readBinary . tr "0*" "10"

formatMasked :: String -> Word32
formatMasked  = readBinary . tr "*" "0"

shiftWidthOfFormat :: String -> Int
shiftWidthOfFormat = length . takeWhile (/= '*') . reverse

fixnumFormat, charFormat, nilFormat  :: String
boolFormat, pairFormat, vectorFormat :: String
stringFormat :: String
fixnumFormat =       "00"
charFormat   = "00001111"
nilFormat    = "00111111"
boolFormat   = "0*101111"
pairFormat   =      "001"
vectorFormat =      "101"
stringFormat =      "110"

nilValue :: Word32
nilValue = readBinary nilFormat

toFixnum :: Integer -> Integer
toFixnum = flip shift $ shiftWidthOfFormat fixnumFormat

toChar :: Char -> Word32
toChar = (.|. formatMasked charFormat)
  . flip shift (shiftWidthOfFormat charFormat)
  . fromIntegral . ord

trueValue, falseValue :: Word32
trueValue = readBinary $ tr "*" "1" boolFormat
falseValue = readBinary $ tr "*" "0" boolFormat

showImmediate :: Word32 -> String
showImmediate n =
  handleValueOfType
    [ (boolFormat, bool "#f" "#t" . (/= 0))
    , (fixnumFormat, show)
    , (charFormat, \i -> "#\\" ++ [chr $ fromIntegral i])
    , (nilFormat, const "()")
    , (pairFormat, mustCompileHandler "pair")
    , (vectorFormat, mustCompileHandler "vector")
    , (stringFormat, mustCompileHandler "string")
    ]

 where
  mustCompileHandler typeName = const $
    printf "To display %ss, execute the resulting binary." typeName

  handleValueOfType :: [(String, Word32 -> String)] -> String
  handleValueOfType = foldr combineHandlers ""
   where
    applyMask mask = n .&. mask
    combineHandlers (format, handler) outputVal
      | outputVal /= "" = outputVal
      | applyMask mask == formatMasked format =
        let actualValue = shift (complement mask .&. n)
                                (negate . shiftWidthOfFormat $ format)
        in handler actualValue
      | otherwise = ""
     where
      mask = maskOfFormat format



