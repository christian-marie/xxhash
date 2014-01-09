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
import qualified Data.Digest.Pure.MD5 as MD5

import Criterion.Main
import Control.DeepSeq
instance NFData M32.Hash32
instance NFData MD5.MD5Digest

main :: IO ()
main = do
    words <- B.readFile "/usr/share/dict/all"
    lazy_words <- L.readFile "/usr/share/dict/all"

    defaultMain 
        [ bgroup "24M text file "
            [ bench "xxhash (haskell)" $ nf XXHash.xxHash' words
            , bench "xxhash-lazy (haskell)" $ nf XXHash.xxHash lazy_words
            , bench "crc32 (c, zlib)" $ nf CRC32.crc32 words
            , bench "adler32 (c, zlib)" $ nf Adler32.adler32 words
            , bench "hashable (c, FNV?)" $ nf Hashable.hash words
            , bench "hashable-lazy (c, FNV?)" $ nf Hashable.hash  lazy_words
            , bench "pureMD5 (haskell, cryptographic)" $ nf MD5.md5 lazy_words
            ]
        ]
