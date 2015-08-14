{-# LANGUAGE BangPatterns #-}
module RGB565 (toRGB565, toRGB565Hex, toHex) where

import Data.Bits (shiftL, shiftR, (.|.))

type Red   = Int
type Green = Int
type Blue  = Int

toRGB565Hex :: (Red, Blue, Green) -> String
toRGB565Hex rgb = toHex (toRGB565 rgb)

toRGB565 :: (Red, Blue, Green) -> Int
toRGB565 (r, g, b) = r' .|. g' .|. b'
  where r'  = (r `shiftR` 3) `shiftL` 11
        g'  = (g `shiftR` 2) `shiftL` 5
        b'  =  b `shiftR` 3

toHex :: Int -> String
toHex x = go x ""
  where go x !acc =
          case quotRem x 16 of
            (q,0) ->       hex q : acc
            (0,r) ->       hex r : acc
            (q,r) -> go q (hex r : acc)

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
