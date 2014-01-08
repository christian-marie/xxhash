{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Digest.XXHash
(
    Acc(..),
    fromAcc,
    stageThree,
    stageTwo,
    stageOne,
    finalize,
    hashByteString,
) where

#include "MachDeps.h"

import Data.Bits (rotateL, shiftR, shiftL, (.|.), xor)
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Data.ByteString.Internal (ByteString(PS), inlinePerformIO)
import Foreign.Storable (peek)

data Acc = Acc { a1 :: !Word32
           , a2 :: !Word32
           , a3 :: !Word32
           , a4 :: !Word32
           } deriving (Eq, Show)

type XXHash = Word32
type Seed = Word32

initState :: Seed -> Acc
initState seed = Acc a1 a2 a3 a4
    where a1 = seed + prime1 + prime2
          a2 = seed + prime2
          a3 = seed
          a4 = seed - prime1

prime1, prime2, prime3, prime4, prime5 :: Word32
prime1 = 2654435761
prime2 = 2246822519
prime3 = 3266489917
prime4 = 668265263
prime5 = 374761393

finalize :: XXHash -> XXHash
finalize hash = step2 `xor` step2 `shiftR` 16
  where
    step1 = (hash `xor` (hash `shiftR` 15)) * prime2
    step2 = (step1 `xor` (step1 `shiftR` 13)) * prime3

stageOne :: Word32 -> Word32 -> Word32 -> Word32 -> Acc -> Acc
stageOne i1 i2 i3 i4 Acc{..} =
    Acc (vx a1 i1) (vx a2 i2) (vx a3 i3) (vx a4 i4)
  where
      vx v i = ((v + i * prime2) `rotateL` 13) * prime1
    
stageTwo :: Word32 -> XXHash -> XXHash
stageTwo i hash =
    ((hash + i * prime3) `rotateL` 17) * prime4

stageThree :: Word8 -> XXHash -> XXHash
stageThree i hash =
    ((hash + fromIntegral i * prime5) `rotateL` 11) * prime1

fromAcc :: Acc -> XXHash
fromAcc Acc{..} = (a1 `rotateL` 1) + (a2 `rotateL` 7) + (a3 `rotateL` 12) + (a4 `rotateL` 18)

{-# INLINE fromAcc #-}
{-# INLINE stageThree #-}
{-# INLINE stageTwo #-}
{-# INLINE stageOne #-}
{-# INLINE finalize #-}
{-# INLINE initState #-}

hashByteString :: Word32 -> ByteString -> XXHash
hashByteString seed (PS fp os len) =
  inlinePerformIO . withForeignPtr fp $ \bs_base_ptr ->
    let ptr_beg = bs_base_ptr `plusPtr` os
        ptr_end = ptr_beg `plusPtr` len
        processBody :: Ptr Word8 -> Acc -> IO XXHash
        processBody ptr !v
            | ptr <= ptr_end `plusPtr` (-16) = do
                i1 <- peekLE32 ptr 0
                i2 <- peekLE32 ptr 4
                i3 <- peekLE32 ptr 8
                i4 <- peekLE32 ptr 12
                processBody (ptr `plusPtr` 16) (stageOne i1 i2 i3 i4 v)
            | otherwise = do
                processEnd ptr $ (fromAcc v) + (fromIntegral len)
        processEnd :: Ptr Word8 -> XXHash -> IO XXHash
        processEnd ptr !hash
            | ptr < ptr_end `plusPtr` (-4) = do
                b <- peekLE32 ptr 0
                processEnd (ptr `plusPtr` 4) (stageTwo b hash)
            | ptr == ptr_end = 
                return $ finalize hash
            | otherwise = do
                b <- peekByte ptr
                processEnd (ptr `plusPtr` 1) (stageThree b hash)
    in if len < 16
       then processEnd ptr_beg $ prime5 + seed + fromIntegral len -- shortcut
       else processBody ptr_beg $ initState seed

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
    let !w = (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|.
            (b1 `shiftL` 8) .|. b0
    return w
#endif
{-# INLINE peekLE32 #-}
