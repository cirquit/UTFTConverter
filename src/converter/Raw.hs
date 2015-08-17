module Raw (toRawFile) where

toRawFile :: [String] -> String
toRawFile l = toRawArray l 1
  where toRawArray :: [String] -> Int -> String
        toRawArray []     _ = []
        toRawArray (x:xs) n = x ++ nl ++ toRawArray xs (n + 1)
           where nl
                  | n `mod` 16 == 0 = "\n"
                  | otherwise       = " "