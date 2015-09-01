-----------------------------------------------------------------------------
-- |
-- Module      :  Format
-- License     :  MIT
-- Maintainer  :  Alexander Isenko <alex.isenko@googlemail.com>
-- Summary of all modules
--
-- * @Format.C@ exports the Platform datatype and the needed formatting for the @.c@ file
-- * @Format.Raw@ exports the pretty prints the array of rgb values to 16 in a row
-- * @Format.RGB565@ exports the functions to convert from a RGB value to RGB565 and the needed hex conversions
-- * @Format.Converter@ exports the functions that create the @.c@ or @.raw@ files when the @FilePath@ is given
------------------------------------------------------------------------------
module Format
  (
    module Format.C
  , module Format.Raw
  , module Format.RGB565
  , module Format.Converter
  ) where

import Format.C
import Format.Raw
import Format.RGB565
import Format.Converter