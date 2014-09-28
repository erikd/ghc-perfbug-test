{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables #-}
module Check.NewX
    ( GmpNewPair (..)
    , testNewInteger
    ) where

import Prelude hiding (Integer)

import Data.Bits ((.&.), shiftR)
import Data.List (intercalate)
import Numeric (showHex)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

import qualified New1.Integer as G
import qualified NewX.GHC.Integer.Type as X
import qualified NewX.Integer as X

import Common.GHC.Integer.Prim

import Check.Helpers


#define NewX   1


testNewInteger :: Spec
testNewInteger = do
    it "Can multiply two small Integers." $ do
        show (X.timesInteger (X.smallInteger 0x100#) (X.smallInteger 0x22#)) `shouldBe` "+0x2200"
        show (X.timesInteger (X.smallInteger -0x100#) (X.smallInteger 0x22#)) `shouldBe` "-0x2200"
        show (X.timesInteger (X.smallInteger 0x100#) (X.smallInteger -0x22#)) `shouldBe` "-0x2200"
        show (X.timesInteger (X.smallInteger -0x100#) (X.smallInteger -0x22#)) `shouldBe` "+0x2200"

    it "Can multiply large Integer by small." $ do
        show (X.timesInteger (X.mkInteger True [0,2]) (X.smallInteger 0x22#)) `shouldBe` "+0x2200000000"
        show (X.timesInteger (X.mkInteger False [0,2]) (X.smallInteger 0x22#)) `shouldBe` "-0x2200000000"
        show (X.timesInteger (X.mkInteger True [0,2]) (X.smallInteger -0x22#)) `shouldBe` "-0x2200000000"
        show (X.timesInteger (X.mkInteger False [0,2]) (X.smallInteger -0x22#)) `shouldBe` "+0x2200000000"

    it "Can multiply two Integers (old failures)." $ do
        show (X.timesInteger (X.mkInteger True [1, 2, 4]) (X.mkInteger True [1, 2])) `shouldBe` "+0x1000000020000000200000001"
        show (X.timesInteger (X.mkInteger True [1, 2, 4, 8]) (X.mkInteger True [1, 2, 4, 8])) `shouldBe` "+0x1000000020000000300000004000000030000000200000001"
        show (X.timesInteger (X.mkInteger True [0x7ffffffe, 0x7ffffffe, 4]) (X.mkInteger True [0x7ffffffe, 0x7ffffffe, 4])) `shouldBe` "+0x18ffffffebffffffb4000000200000004"
        show (X.timesInteger (X.mkInteger False [0, 0x7fffffff]) (X.mkInteger False [0, 0xfffffffe])) `shouldBe` "+0x1fffffff800000008000000000000000"
        show (X.timesInteger (X.mkInteger False [1, 0x7fffffff]) (X.mkInteger False [1, 0xfffffffe])) `shouldBe` "+0x1fffffff800000013ffffffe80000001"
        show (X.timesInteger (X.mkInteger False [0x3b129743, 0x6b866650]) (X.mkInteger False [0x18865e53,0x6295e0a])) `shouldBe` "+0xa5a19af9c4da2c1eaac6f46fa3a4b9"
        show (X.timesInteger (X.mkInteger True [1, 1, 6]) (X.mkInteger True [1, 1, 6])) `shouldBe` "+0x240000001800000034000000100000001"
        show (X.timesInteger (X.mkInteger True [ 0, 0, 0, 4, 8, 1 ]) (X.mkInteger True [ 0, 0, 0, 8 ])) `shouldBe` "+0x800000080000000800000000000000000000000000000000000000000000000"
        show (X.timesInteger (X.mkInteger True [0, 2, 4, 8, 16, 32]) (X.mkInteger True [0,2])) `shouldBe` "+0x1000000010000000100000001000000010000000000000000"
        show (X.timesInteger (X.mkInteger True [0, 1, 1, 1, 1 , 1]) (X.mkInteger True [0, 1, 0, 0, 1])) `shouldBe` "+0x8000000100000002000000080000001000000010000000200000004000000000000000"
        let a4 = [0x7ffffc3d,0x7ffffdc4,0x7ffffeab]
            b4 = [0x7fffff03,0x7ffffc71,0x7fffff6e,0x7ffffd6d,0x7fffff7a,0x294]
        show (X.timesInteger (X.mkInteger True a4) (X.mkInteger True b4)) `shouldBe` "+0x294fff9232dffebaeebffd6dbf80086cfb001f4c78002d7cc4007c9bc8003b7b7"
        let a5 = [1,0,4,0,16,0,64,0,256,0,1024]
            b5 = [1,0,0,0,32,0,192,0,1024]
        show (X.timesInteger (X.mkInteger True a5) (X.mkInteger True b5)) `shouldBe` "+0x4000000000000000700000000000000090000000000000009000000000000000a000000000000000a0000000000000006000000000000000300000000000000010000000000000001"


    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (X.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

    it "Can calculate product [1..n]." $ do
        show (foldl1 X.timesInteger $ map (\x -> X.smallInteger (unboxInt x)) [1..10])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..10])
        show (foldl1 X.timesInteger $ map (\x -> X.smallInteger (unboxInt x)) [1..100])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..100])

    it "Get correct result at boundaries." $ do
        let maxSmall = X.wordToInteger (unboxWord 0xffffffffffffffff)
            twoSmall = X.wordToInteger (unboxWord 2)
        show (X.timesInteger maxSmall twoSmall) `shouldBe` "+0x1fffffffffffffffe"
        show (X.timesInteger twoSmall maxSmall) `shouldBe` "+0x1fffffffffffffffe"

        let allBitsSet = X.mkInteger True (replicate 8 0x7fffffff ++ [0xff])
        show (X.timesInteger allBitsSet allBitsSet) `shouldBe` "+0x" ++ replicate 63 'f' ++ "e" ++ replicate 63 '0' ++ "1"

    prop "Muliplication results are minimal." $ \ (GNP _ a, GNP _ b) ->
        X.isMinimal (X.timesInteger a b) `shouldBe` True


--------------------------------------------------------------------------------

data GmpNewPair
    = GNP G.Integer X.Integer

instance Show GmpNewPair where
    show (GNP g x) =
        if show g /= show x
            then error $ "show GmpNewPair error " ++ show g ++ " /= " ++ show x
            else toMakeInteger x


toMakeInteger :: X.Integer -> String
toMakeInteger xi =
    let s = X.hexShow xi
        nonNeg = case head s of
                    '-' -> False
                    _ -> True
        i = readInteger (if head s == '0' then s else tail s)
    in "mkInteger " ++ show nonNeg ++ " [" ++ wrds i ++ "]"
  where
    wrds i =
        intercalate "," . map (\w -> "0x" ++ showHex w "") $ decompose i
    decompose x
        | x <= 0 = []
        | otherwise =
            x .&. 0x7fffffff : decompose (x `shiftR` 31)

instance Arbitrary GmpNewPair where
    arbitrary = do
        bool <- arbitrary
        if bool
            then do
                i <- fmap getNonZero arbitrary
                return $! GNP (G.smallInteger (unboxInt i)) (X.smallInteger (unboxInt i))
            else do
                sign <- arbitrary
                pos <- fmap (positive32bits . take 30 . nonEmptyNonZero) arbitrary
                return $! GNP (G.mkInteger sign pos) (X.mkInteger sign pos)
    shrink (GNP g x) =
        map (\i -> GNP (G.shiftLInteger g (unboxInt i)) (X.shiftLInteger x (unboxInt i))) [1..50]

newtype NonEmptyNonZero a = NonEmptyNonZero { nonEmptyNonZero :: [a] }

instance (Arbitrary a, Eq a, Num a, Ord a) => Arbitrary (NonEmptyNonZero a) where
    arbitrary = do
        x <- fmap getNonZero arbitrary
        xs <- arbitrary
        return $ NonEmptyNonZero (x:xs)

    shrink (NonEmptyNonZero xs) =
        [ NonEmptyNonZero xs'
        | xs' <- shrink xs
        , not (null xs')
        ]
