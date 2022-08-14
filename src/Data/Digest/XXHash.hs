{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-orphans #-}

-- |
-- Module      : Data.Digest.XXHash
-- Copyright   : (c) Christian Marie 2013
-- License     : BSD3
--
-- Maintainer  : pingu@anchor.net.au
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a pure implementation of the xxHash algorithm.
--
-- This implementation is almost as fast as the C version, which is avaliable
-- as c_xxHash'.
--
-- Criterion benchmarks are avaliable via cabal bench.
--
-- More information on the algorithm may be found here:
-- <https://code.google.com/p/xxhash/>
--
module Data.Digest.XXHash
(
    -- *Types
    XXHash,
    -- *Haskell implementation
    xxHash,
    xxHash',
    -- *C bindings
    c_xxHash',
) where

#include "MachDeps.h"

import Crypto.Classes (Hash (..), hash)
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString (PS))
import qualified Data.ByteString.Lazy as L
import qualified Data.Digest.CXXHash as C
import Data.Tagged
import Data.Word (Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

-- These all get unboxed. Previous versions used unboxed words, tuples and
-- primitive operations directly. This is much more readable for only a slight
-- performance penalty (less than 5%)
data XXHashCtx = XXHashCtx { a1         :: !Word32 -- ^ Accumulators
                           , a2         :: !Word32
                           , a3         :: !Word32
                           , a4         :: !Word32
                           , iterations :: !Word32 -- ^ To keep track length
                           } deriving (Eq, Show)

type XXHash = Word32

-- The user can't currently touch the seed, it is set to zero in the Hash
-- instance definition
type Seed = Word32

initializeXXHashCtx :: Seed -> XXHashCtx
initializeXXHashCtx seed =
    XXHashCtx a1 a2 a3 a4 iterations
  where a1 = seed + prime1 + prime2
        a2 = seed + prime2
        a3 = seed
        a4 = seed - prime1
        iterations = 0

prime1, prime2, prime3, prime4, prime5 :: Word32
prime1 = 2654435761
prime2 = 2246822519
prime3 = 3266489917
prime4 = 668265263
prime5 = 374761393

-- This is inlined without any explicit help, the inline here is maybe
-- redundant.
stageOne :: Word32 -> Word32 -> Word32 -> Word32 -> XXHashCtx -> XXHashCtx
stageOne i1 i2 i3 i4 XXHashCtx{..} =
    XXHashCtx (vx a1 i1) (vx a2 i2) (vx a3 i3) (vx a4 i4) (iterations + 1)
  where
      vx v i = ((v + i * prime2) `rotateL` 13) * prime1
{-# INLINE stageOne #-}

-- This is always called with a multiple of sixteen, convenient.
updateXXHashCtx :: XXHashCtx -> ByteString -> XXHashCtx
updateXXHashCtx ctx (PS fp os len) =
    unsafePerformIO . withForeignPtr fp $ \bs_base_ptr ->
        let ptr_beg = bs_base_ptr `plusPtr` os
            ptr_end = ptr_beg `plusPtr` len
            go ptr !ctx'
                | ptr < ptr_end = do
                    i1 <- peekLE32 ptr 0
                    i2 <- peekLE32 ptr 4
                    i3 <- peekLE32 ptr 8
                    i4 <- peekLE32 ptr 12
                    go (ptr `plusPtr` 16) (stageOne i1 i2 i3 i4 ctx')
                | otherwise = return ctx'
        in go ptr_beg ctx

finalizeXXHashCtx :: Seed -> XXHashCtx -> ByteString -> XXHash
finalizeXXHashCtx seed ctx (PS fp os len) =
    unsafePerformIO . withForeignPtr fp $ \bs_base_ptr ->
        let ptr_beg = bs_base_ptr `plusPtr` os
            ptr_end = ptr_beg `plusPtr` len
            total_len :: Word64
            total_len = fromIntegral len + fromIntegral (iterations ctx) * 16
            go :: Ptr Word8 -> XXHashCtx -> IO XXHash
            go ptr !ctx'
                | ptr /= nullPtr && ptr <= ptr_end `plusPtr` (-16) = do
                    i1 <- peekLE32 ptr 0
                    i2 <- peekLE32 ptr 4
                    i3 <- peekLE32 ptr 8
                    i4 <- peekLE32 ptr 12
                    go (ptr `plusPtr` 16) (stageOne i1 i2 i3 i4 ctx')
                | otherwise =
                    goEnd ptr $
                        if total_len >= 16
                        then ctxToDigest ctx' total_len
                        else  seed + prime5 + fromIntegral total_len
            goEnd :: Ptr Word8 -> XXHash -> IO XXHash
            goEnd ptr !xxhash
                | ptr /= nullPtr && ptr <= ptr_end `plusPtr` (-4) = do
                    b <- peekLE32 ptr 0
                    goEnd (ptr `plusPtr` 4) (stageTwo b xxhash)
                | ptr == ptr_end =
                    return $ finalizeXXHash xxhash
                | otherwise = do
                    b <- peekByte ptr
                    goEnd (ptr `plusPtr` 1) (stageThree b xxhash)
        in go ptr_beg ctx
  where
    stageTwo i xxhash =
        ((xxhash + i * prime3) `rotateL` 17) * prime4

    stageThree i xxhash =
        ((xxhash + fromIntegral i * prime5) `rotateL` 11) * prime1

    ctxToDigest XXHashCtx{..} total_len =
        (a1 `rotateL` 1) + (a2 `rotateL` 7) +
        (a3 `rotateL` 12) + (a4 `rotateL` 18) + fromIntegral total_len

    finalizeXXHash xxhash =  step2 `xor` step2 `shiftR` 16
      where
        step1 = (xxhash `xor` (xxhash `shiftR` 15)) * prime2
        step2 = (step1 `xor` (step1 `shiftR` 13)) * prime3

-- PeekByte and peekLE32 are more or less borrowed and modified code from
-- this:
-- http://www.serpentine.com/blog/2012/10/02/a-fast-new-siphash-implementation-in-haskell/
peekByte :: Ptr Word8 -> IO Word8
peekByte = peek
{-# INLINE peekByte #-}

peekLE32 :: Ptr Word8 -> Int -> IO Word32
#if defined(x86_64_HOST_ARCH) || defined(i386_HOST_ARCH)
peekLE32 ptr os = peek (castPtr (ptr `plusPtr` os))
#else
-- Super slow, around 100x slower.
peekLE32 ptr os = do
    let peek8 d = fromIntegral `fmap` peekByte (ptr `plusPtr` (d + os))
    b0 <- peek8 0
    b1 <- peek8 1
    b2 <- peek8 2
    b3 <- peek8 3
    let w = (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|.
            (b1 `shiftL` 8) .|. b0
    return w
#endif
{-# INLINE peekLE32 #-}

-- Crypto.Classes does our boring stuff for us :D
instance Hash XXHashCtx XXHash where
    outputLength = Tagged 32 -- Word32
    -- 128 bits, meaning that the context will never be updated with less
    -- than 16 bytes
    blockLength  = Tagged 128
    initialCtx   = initializeXXHashCtx 0
    updateCtx    = updateXXHashCtx
    finalize     = finalizeXXHashCtx 0


-- |
-- Hash a lazy ByteString.
xxHash :: L.ByteString -> XXHash
xxHash = hash

-- |
-- Hash a strict ByteString.
xxHash' :: ByteString -> XXHash
xxHash' = hash'

-- |
-- Hash a strict ByteString using the C implementation, the length of the
-- ByteString should be limited to 2^31-1 or the results will be invalid.
--
-- This is mostly used internally for benchmarking and verification. It's use
-- in production is not recommended.
c_xxHash' :: B.ByteString -> XXHash
c_xxHash' bs = unsafePerformIO . B.useAsCStringLen bs $ \(str, len) ->
    C.c_XXH32 str (fromIntegral len) 0
