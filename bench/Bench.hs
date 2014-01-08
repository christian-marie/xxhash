module Main
       ( main -- :: IO ()
       ) where
import Prelude hiding (words)

import qualified Data.ByteString as S
import qualified Data.Digest.XXHash as XXHash
import qualified Data.Digest.Adler32 as Adler32
import qualified Data.Digest.CRC32 as CRC32

import Criterion.Main

main :: IO ()
main = do
    words <- S.readFile "/usr/share/dict/words"

    defaultMain 
        [ bgroup "/usr/share/dict/words"
            [ bench "xxhash" $ nf (XXHash.hashByteString 0) words
            , bench "crc32" $ nf CRC32.crc32 words
            , bench "adler32" $ nf Adler32.adler32 words
            ]
        ]
