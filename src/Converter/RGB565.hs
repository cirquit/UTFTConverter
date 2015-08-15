{-# LANGUAGE BangPatterns #-}
module RGB565 (toRGB565, toRGB565Hex, to4Hex, toHex) where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word (Word8())

toRGB565Hex :: (Word8, Word8, Word8) -> String
toRGB565Hex rgb = to4Hex (toRGB565 rgb)

toRGB565 :: (Word8, Word8, Word8) -> Int
toRGB565 (r, g, b) = r' .|. g' .|. b'
  where r'  = ((toInt r) `shiftR` 3) `shiftL` 11
        g'  = ((toInt g) `shiftR` 2) `shiftL` 5
        b'  =  (toInt b) `shiftR` 3

toInt :: Word8 -> Int
toInt = fromIntegral

toHex :: Int -> String
toHex x = go x ""
  where go x !acc =
          case quotRem x 16 of
            (0,r) ->       hex r : acc
            (q,r) -> go q (hex r : acc)

to4Hex :: Int -> String
to4Hex x = go hx
  where hx = toHex x
        go :: String -> String
        go str
          | length str < 4 = go ('0':str)
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
hex x  = error $ "This was unexpected in 'hex' - " ++ show x