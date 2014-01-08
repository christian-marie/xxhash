{-# LANGUAGE OverloadedStrings #-}

module TestSuite where

import Test.Hspec

import Data.Digest.XXHash

suite :: Spec
suite = do
    describe "Internal functions, of which:" internalFunctionsTest
    describe "Hashing a bytestring" $
        it "works with known pairs" $ do
            hashByteString 0 "the quick brown fox jumps over a pony" `shouldBe`
                2069436874
            hashByteString 0 "small" `shouldBe`
                2387791210

internalFunctionsTest :: Spec
internalFunctionsTest = do
    it "fromV has correct output" $
        3672751773 `shouldBe` fromV 
            (V 3707864141 1226176970 2466439235 1709939193)
    it "stageOne has correct output" testStageOne
    it "stageTwo has correct output" $
        stageTwo 1852796960 3672751810 `shouldBe` 892131078
    it "stageThree has correct output" $
        stageThree 121 892131078 `shouldBe` 1167152160
    it "finalize has correct output" $
        finalize 1167152160 `shouldBe` 2069436874
  where
    testStageOne =
        r `shouldBe` stageOne 543516788 1667855729 1919033451 544110447 v
      where
        
        v = V 606290984 2246822519 0 1640531535
        r = V 2590359605 2072081322 3418880061 977135912
