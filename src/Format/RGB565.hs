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
module Format.RGB565 (toRGB565, toRGB565Hex, to6Hex, to4Hex, toHex) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word (Word8())


-- | toRGB565Hex takes a RGB value and converts it into a RGB565 encoded 4 digit hex String
--
-- __Example usage:__
--
-- @
-- λ> toRGB565Hex (255, 0, 0)
-- \"F800\"
-- @

toRGB565Hex :: (Word8, Word8, Word8) -> String
toRGB565Hex rgb = to4Hex (toRGB565 rgb)

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

-- | toHex takes an Int and converts it into a hex String
--
-- __Example usage:__
--
-- @
-- λ> toHex 255
-- \"F8\"
-- @
toHex :: Int -> String
toHex = go ""
  where go !acc x =
          case quotRem x 16 of
            (0,r) ->     hex r : acc
            (q,r) -> go (hex r : acc) q

-- | to4Hex takes an Int and converts it to a hex String and adds zeros until four digits are filled
--
-- __Example usage:__
--
-- @
-- λ> to4Hex 255
-- "00F8"
-- @
to4Hex :: Int -> String
to4Hex x = go hx
  where hx = toHex x
        go :: String -> String
        go !str
          | length str < 4 = go ('0':str)
          | otherwise      = str

-- | to6Hex takes an Int and converts it to a hex String and adds zeros until six digits are filled
--
-- __Example usage:__
--
-- @
-- λ> to4Hex 255
-- "0000F8"
-- @
to6Hex :: Int -> String
to6Hex x = go hx
  where hx = toHex x
        go :: String -> String
        go !str
          | length str < 6 = go ('0':str)
          | otherwise      = str



hex :: Int -> Char
hex 0  = '0'
hex 1  = '1'
hex 2  = '2'
hex 3  = '3'
hex 4  = '4'
hex 5  = '5'
hex 6  = '6'
hex 7  = '7'
hex 8  = '8'
hex 9  = '9'
hex 10 = 'A'
hex 11 = 'B'
hex 12 = 'C'
hex 13 = 'D'
hex 14 = 'E'
hex 15 = 'F'
hex x  = error $ "Exceeded bounds of 'hex' - " ++ show x