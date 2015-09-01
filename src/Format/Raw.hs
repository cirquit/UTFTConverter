{-# LANGUAGE BangPatterns #-}

module Format.Raw (toRawFile) where

toRawFile :: [String] -> String
toRawFile l = toRawArray l 1 []
  where toRawArray :: [String] -> Int -> String -> String
        toRawArray []     _  acc = acc
        toRawArray (x:xs) n !acc = toRawArray xs (n + 1) (acc ++ x ++ nl)
           where nl
                  | n `mod` 16 == 0 = "\n"
                  | otherwise       = " "