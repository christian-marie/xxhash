{-# LANGUAGE OverloadedStrings, UnboxedTuples, MagicHash #-}

module TestSuite where

import Test.Hspec

import Data.Digest.XXHash
import GHC.Word

suite :: Spec
suite = do
    describe "Internal functions, of which:" internalFunctionsTest
    describe "Hashing a bytestring" $
        it "works with known pairs" $ do
            hashByteString 0 "the quick brown fox jumps over a pony" `shouldBe`
                2069436874
            hashByteString 0 "small" `shouldBe` 2387791210
            hashByteString 123 "test" `shouldBe` 2758658570
            hashByteString 42 "b" `shouldBe` 2220134325

internalFunctionsTest :: Spec
internalFunctionsTest = do
    it "fromAcc has correct output" $
        3672751773 `shouldBe` fromAcc 3707864141 1226176970 2466439235 1709939193
    it "stageOne has correct output" testStageOne
    it "stageTwo has correct output" $
        stageTwo 1852796960 3672751810 `shouldBe` 892131078
    it "stageThree has correct output" $
        stageThree 121 892131078 `shouldBe` 1167152160
    it "finalize has correct output" $
        finalize 1167152160 `shouldBe` 2069436874
  where
    testStageOne = do
        let (# a1, a2, a3, a4 #) = stageOne 543516788## 1667855729## 1919033451## 544110447##
                                            606290984## 2246822519## 0##          1640531535## 
        W32# a1 `shouldBe` 2590359605
        W32# a2 `shouldBe` 2072081322
        W32# a3 `shouldBe` 3418880061
        W32# a4 `shouldBe` 977135912
