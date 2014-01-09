module Main
       ( main -- :: IO ()
       ) where
import Prelude hiding (words)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Digest.XXHash as XXHash
import qualified Data.Digest.Adler32 as Adler32
import qualified Data.Digest.CRC32 as CRC32

import Criterion.Main

main :: IO ()
main = do
    words <- B.readFile "/usr/share/dict/words"
    lazy_words <- L.readFile "/usr/share/dict/words"

    defaultMain 
        [ bgroup "/usr/share/dict/words"
            [ bench "xxhash" $ nf XXHash.xxHash' words
            , bench "xxhash-lazy" $ nf XXHash.xxHash lazy_words
            , bench "crc32" $ nf CRC32.crc32 words
            , bench "adler32" $ nf Adler32.adler32 words
            ]
        ]
