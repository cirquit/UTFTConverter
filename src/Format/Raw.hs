{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Format
-- License     :  MIT
-- Maintainer  :  Alexander Isenko <alex.isenko@googlemail.com>
--
-- @Format.Raw@ exports the pretty prints the array of rgb values to 16 in a row
------------------------------------------------------------------------------

module Format.Raw (toRawFile) where

-- | toRawFile takes the hex strings and returns a concatinated string with a @\\n@
--   after every 16th hex number
--
-- __Example usage:__
--
-- @
-- λ> let hex = ["0000", "0000", \"FF00\", "00FF","0000", "0000", \"FF00\", "00FF", "0000", "0000", \"FF00\", "00FF", "0000", "0000", \"FF00\", "00FF", "0000"]
-- λ> toRawFile hex
-- "0000 0000 FF00 00FF 0000 0000 FF00 00FF 0000 0000 FF00 00FF 0000 0000 FF00 00FF \\n 0000 "
-- @
toRawFile :: [String] -> String
toRawFile l = toRawArray l 1 []
  where toRawArray :: [String] -> Int -> String -> String
        toRawArray []     _  acc = acc
        toRawArray (x:xs) n !acc = toRawArray xs (n + 1) (acc ++ x ++ nl)
           where nl
                  | n `mod` 16 == 0 = "\n"
                  | otherwise       = " "