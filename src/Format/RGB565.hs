{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Format.RGB565
-- License     :  MIT
-- Maintainer  :  Alexander Isenko <alex.isenko@googlemail.com>
--
-- @Format.RGB565@ exports the functions to convert from a RGB value to RGB565 and the needed hex conversions
--
-----------------------------------------------------------------------------
module Format.RGB565 (toRGB565, toRGB565Hex, toNHex, to6Hex, to4Hex, toHex) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word (Word8())
import Numeric   (showHex)
import Data.Char (toUpper)


-- | toRGB565Hex takes a RGB value and converts it into a RGB565 encoded 4 digit hex String
--
-- __Example usage:__
--
-- @
-- λ> toRGB565Hex (255, 0, 0)
-- \"F800\"
-- @

toRGB565Hex :: (Word8, Word8, Word8) -> String
toRGB565Hex rgb = toNHex 4 (toRGB565 rgb)

-- | toRGB565 takes a RGB value and converts it into a RGB565 encoded Int
--
-- __Example usage:__
--
-- @
-- λ> toRGB565 (255, 0, 0)
-- 63488
-- @

toRGB565 :: (Word8, Word8, Word8) -> Int
toRGB565 (r, g, b) = r' .|. g' .|. b'
  where r'  = ((toInt r) `shiftR` 3) `shiftL` 11
        g'  = ((toInt g) `shiftR` 2) `shiftL` 5
        b'  =  (toInt b) `shiftR` 3

toInt :: Word8 -> Int
toInt = fromIntegral

-- | toHex takes an unsigned Int and converts it into a hex String
--
-- __Example usage:__
--
-- @
-- λ> toHex 255
-- \"FF\"
-- @
toHex :: Int -> String
toHex = map toUpper . flip showHex ""

-- | toNHex takes the desired stringlength __n__ and an unsigned Int and converts it to a hex String and adds zeros until __n__ digits are filled
--
-- __Example usage:__
--
-- @
-- λ> toNHex 4 255
-- "00FF"
--
-- λ> toNHex 6 255
-- "0000FF"
-- @
toNHex :: Int -> Int -> String
toNHex n x = map toUpper $ go hx
  where hx = toHex x
        go :: String -> String
        go !str
          | length str < n = go ('0':str)
          | otherwise      = str


-- | to4Hex takes an unsigned Int and converts it to a hex String and adds zeros until four digits are filled
--
-- __Example usage:__
--
-- @
-- λ> to4Hex 255
-- "00FF"
-- @
{-# DEPRECATED to4Hex "Use toNHex 4 instead" #-}
to4Hex :: Int -> String
to4Hex x = map toUpper $ go hx
  where hx = toHex x
        go :: String -> String
        go !str
          | length str < 6 = go ('0':str)
          | otherwise      = str

-- | to4Hex takes an unsigned Int and converts it to a hex String and adds zeros until six digits are filled
--
-- __Example usage:__
--
-- @
-- λ> to6Hex 255
-- "0000FF"
-- @
{-# DEPRECATED to6Hex "Use toNHex 6 instead" #-}
to6Hex :: Int -> String
to6Hex x = map toUpper $ go hx
  where hx = toHex x
        go :: String -> String
        go !str
          | length str < 6 = go ('0':str)
          | otherwise      = str