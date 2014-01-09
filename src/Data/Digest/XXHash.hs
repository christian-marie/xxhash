{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-orphans #-}

module Data.Digest.XXHash
(
    XXHashCtx,
    XXHash,
    xxHash,
    xxHash',
) where

#include "MachDeps.h"

import Data.Bits
import Data.Tagged
import Data.Word (Word8, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Data.ByteString.Internal (ByteString(PS), inlinePerformIO)
import Foreign.Storable (peek)
import Foreign.Ptr (nullPtr)
import Crypto.Classes (Hash(..), hash)
import qualified Data.ByteString.Lazy as L

data XXHashCtx = XXHashCtx { a1         :: !Word32
                           , a2         :: !Word32
                           , a3         :: !Word32
                           , a4         :: !Word32
                           , iterations :: !Word32
                           } deriving (Eq, Show)

type XXHash = Word32
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

stageOne :: Word32 -> Word32 -> Word32 -> Word32 -> XXHashCtx -> XXHashCtx
stageOne i1 i2 i3 i4 XXHashCtx{..} =
    XXHashCtx (vx a1 i1) (vx a2 i2) (vx a3 i3) (vx a4 i4) (iterations + 1)
  where
      vx v i = ((v + i * prime2) `rotateL` 13) * prime1

-- This could be a lot faster, if I could figure out how. Suggestions welcome.
updateXXHashCtx :: XXHashCtx -> ByteString -> XXHashCtx
updateXXHashCtx ctx (PS fp os len) =
    inlinePerformIO . withForeignPtr fp $ \bs_base_ptr ->
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
    inlinePerformIO . withForeignPtr fp $ \bs_base_ptr ->
        let ptr_beg = bs_base_ptr `plusPtr` os
            ptr_end = ptr_beg `plusPtr` len
            total_len :: Word64
            total_len = (fromIntegral len) + (fromIntegral $ iterations ctx) * 16
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
                        else  seed + prime5 + (fromIntegral total_len)
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
        (a1 `rotateL` 1) + (a2 `rotateL` 7) + (a3 `rotateL` 12) + (a4 `rotateL` 18) + (fromIntegral total_len)
    finalizeXXHash xxhash =  step2 `xor` step2 `shiftR` 16
      where
        step1 = (xxhash `xor` (xxhash `shiftR` 15)) * prime2
        step2 = (step1 `xor` (step1 `shiftR` 13)) * prime3

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

instance Hash XXHashCtx XXHash where
	outputLength = Tagged 32 -- Word32
        -- 128 bits, meaning that the context will never be updated with less
        -- than 16 bytes
	blockLength  = Tagged 128 
	initialCtx   = initializeXXHashCtx 0
	updateCtx    = updateXXHashCtx
	finalize     = finalizeXXHashCtx 0

xxHash :: L.ByteString -> XXHash
xxHash = hash

xxHash' :: ByteString -> XXHash
xxHash' = hash'
