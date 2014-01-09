{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestSuite where

import Debug.Trace
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Digest.XXHash
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

instance Arbitrary L.ByteString where
    arbitrary = L.pack `fmap` arbitrary
    shrink xs = L.pack `fmap` shrink (L.unpack xs)

suite :: Spec
suite = do
    describe "Hashing a bytestring" $ do
        it "does not explode with sizes up to 2048" $ do
            let strings = map ($ repeat 'A') (map (\n -> take n) [1..2048]) in
                (length $ map (xxHash' . B.pack) strings) `shouldBe` 2048

        it "works with known pairs" $ do
            xxHash "" `shouldBe` 46947589
            xxHash "the quick brown fox jumps over a pony" `shouldBe` 2069436874
            xxHash' "small" `shouldBe` 2387791210
            xxHash' "" `shouldBe` 46947589
            let as = B.pack . take 500 $ repeat 'A' in
                xxHash' as  `shouldBe` 1719491600

    describe "Fuzzing random strings" $ do
       prop "is deterministic" $  \string -> trace (show string) $
            let h = xxHash string in
                (xxHash' $ L.toStrict string) == h


