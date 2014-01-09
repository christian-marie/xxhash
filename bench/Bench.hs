{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
       ( main -- :: IO ()
       ) where
import Prelude hiding (words)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Digest.XXHash as XXHash
import qualified Data.Digest.Adler32 as Adler32
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Digest.Murmur32 as M32
import qualified Data.Hashable as Hashable

import Criterion.Main
import Control.DeepSeq
instance NFData M32.Hash32

main :: IO ()
main = do
    med <- L.readFile "/usr/share/dict/all"
    med' <- B.readFile "/usr/share/dict/all"

    small <- L.readFile "/usr/share/dict/cracklib-small"
    small' <- B.readFile "/usr/share/dict/cracklib-small"

    defaultMain 
        [ bgroup "24MB text"
            [ bench "xxhash (c)" $ nf XXHash.c_xxHash' med'
            , bench "Data.Digest.XXHash lazy (haskell)" $ nf XXHash.xxHash med
            , bench "Data.Digest.XXHash (haskell)" $ nf XXHash.xxHash' med'
            , bench "Data.Digest.adler32 (c, zlib)" $ nf Adler32.adler32 med'
            , bench "Data.Digest.crc32 (c, zlib)" $ nf CRC32.crc32 med'
            , bench "Data.Hashable (c, FNV?)" $ nf Hashable.hash med'
            ]
        , bgroup "478K text"
            [ bench "Data.Digest.XXHash lazy (haskell)" $ nf XXHash.xxHash small
            , bench "Data.Digest.XXHash (haskell)" $ nf XXHash.xxHash' small'
            , bench "xxhash (c)" $ nf XXHash.c_xxHash' small'
            , bench "Data.Digest.adler32 (c, zlib)" $ nf Adler32.adler32 small'
            , bench "Data.Digest.crc32 (c, zlib)" $ nf CRC32.crc32 small'
            , bench "Data.Hashable (c, FNV?)" $ nf Hashable.hash small'
            ]
        ]
