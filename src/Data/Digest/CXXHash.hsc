{-# LANGUAGE CPP #-}
module Data.Digest.CXXHash
(
    c_XXH32
) where

import qualified Data.ByteString as B
import Foreign.C.String
import Foreign.C.Types
import Data.Word(Word32)

#include <xxhash.h>
-- | Compresses a string.
foreign import ccall unsafe "xxhash.h XXH32"
  c_XXH32 :: CString -- ^ Data
          -> CSize   -- ^ Size
          -> CUInt   -- ^ Seed
          -> IO Word32   -- ^ Seed
