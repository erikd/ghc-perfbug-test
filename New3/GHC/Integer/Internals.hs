{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


#include "MachDeps.h"

module New3.GHC.Integer.Internals
{-
    ( Integer (..)
    , mkInteger, smallInteger, wordToInteger, integerToWord, integerToInt
#if WORD_SIZE_IN_BITS < 64
    , integerToWord64, word64ToInteger
    , integerToInt64, int64ToInteger
#endif
    , plusInteger, minusInteger, timesInteger, negateInteger
    , eqInteger, neqInteger, absInteger, signumInteger
    , leInteger, gtInteger, ltInteger, geInteger, compareInteger
    , divModInteger, quotRemInteger, quotInteger, remInteger
    , encodeFloatInteger, decodeFloatInteger, floatFromInteger
    , encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger
    -- , gcdInteger, lcmInteger -- XXX
    , andInteger, orInteger, xorInteger, complementInteger
    , shiftLInteger, shiftRInteger
    , hashInteger


    , toList

    ) where
-}
    where

import Prelude hiding (Integer, abs, pi, sum, rem, succ) -- (all, error, otherwise, return, show, (++))

import Data.Bits

import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Numeric (showHex) -- TODO: Remove when its working.

import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import New3.GHC.Integer.Natural
import New3.GHC.Integer.Sign
import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray

#if !defined(__HADDOCK__)

--------------------------------------------------------------------------------

mkInteger :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Integer
mkInteger _ [] = zeroInteger
mkInteger True [I# i] = smallInteger i
mkInteger False [I# i] = smallInteger (negateInt# i)
mkInteger nonNegative is =
    let abs = f is
    in if nonNegative
        then abs
        else negateInteger abs
  where
    f [] = zeroInteger
    f [I# x] = smallInteger x
    f (I# x : xs) = smallInteger x `orInteger` shiftLInteger (f xs) 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i
    | isTrue# (i ==# 0#) = zeroInteger
    | isTrue# (i <# 0#) = SmallNeg (int2Word# (negateInt# i))
    | otherwise = SmallPos (int2Word# i)

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = SmallPos w

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (SmallPos w) = w
integerToWord (SmallNeg w) = w
integerToWord (Positive _ arr) = unboxWord (indexWordArray arr 0)
integerToWord (Negative _ arr) = unboxWord (indexWordArray arr 0)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (SmallPos w) = word2Int# w
integerToInt (SmallNeg w) = negateInt# (word2Int# w)
integerToInt (Positive _ arr) = firstWordAsInt Pos arr
integerToInt (Negative _ arr) = firstWordAsInt Neg arr

firstWordAsInt :: Sign -> WordArray -> Int#
firstWordAsInt s arr =
    let i = word2Int# (unboxWord (indexWordArray arr 0))
    in case s of
        Pos -> i
        Neg -> negateInt# i


{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (SmallPos a) (SmallPos b) = SmallPos (or# a b)
orInteger (SmallPos a) (SmallNeg b) = SmallNeg (plusWord# 1## (and# (not# a) (minusWord# b 1##)))
orInteger (SmallNeg a) (SmallPos b) = SmallNeg (plusWord# 1## (and# (minusWord# a 1##) (not# b)))
orInteger (SmallNeg a) (SmallNeg b) = SmallNeg (plusWord# 1## (and# (minusWord# a 1##) (minusWord# b 1##)))

orInteger (SmallPos a) (Positive n arr) = fromNatural Pos (orNaturalW (Natural n arr) (W# a))
orInteger (Positive n arr) (SmallPos b) = fromNatural Pos (orNaturalW (Natural n arr) (W# b))

orInteger (Positive n1 arr1) (Positive n2 arr2) = fromNatural Pos (orNatural (Natural n1 arr1) (Natural n2 arr2))

orInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))


{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger !a 0# = a
shiftLInteger !(SmallPos !a) b
    | isTrue# (eqWord# a 0##) = zeroInteger
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = fromNatural Pos (shiftLNatural (mkSingletonNat (W# a)) (I# b))
    | otherwise =
        let !lo = unsafeShiftL (W# a) (I# b)
            hi = unsafeShiftR (W# a) (I# ( WORD_SIZE_IN_BITS# -# b))
        in if hi == 0
            then SmallPos (unboxWord lo)
            else mkPair Positive lo hi

shiftLInteger !(SmallNeg !a) !b = fromNatural Neg (shiftLNatural (mkSingletonNat (W# a)) (I# b))
shiftLInteger !(Positive !n !arr) !b = fromNatural Pos (shiftLNatural (Natural n arr) (I# b))
shiftLInteger !(Negative !n !arr) !b = fromNatural Neg (shiftLNatural (Natural n arr) (I# b))

smallShiftLArray :: Int -> WordArray -> (# Int, Int #) -> Natural
smallShiftLArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (n + 1)
    nlen <- loop marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural nlen narr
  where
    loop !marr !i !mem
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftL x si) .|. mem)
            loop marr (i + 1) (unsafeShiftR x sj)
        | mem /= 0 = do
            writeWordArray marr i mem
            return $ i + 1
        | otherwise = return n

-- | TODO : Use copy here? Check benchmark results.
wordShiftLArray :: Int -> WordArray -> Int -> Natural
wordShiftLArray !n !arr !q = runStrictPrim $ do
    marr <- newWordArray (n + q)
    loop1 marr 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural (n + q) narr
  where
    loop1 !marr !i
        | i < q = do
            writeWordArray marr i 0
            loop1 marr (i + 1)
        | otherwise = loop2 marr 0
    loop2 !marr !i
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr (q + i) x
            loop2 marr (i + 1)
        | otherwise = return ()

largeShiftLArray :: Int -> WordArray-> (# Int, Int, Int #) -> Natural
largeShiftLArray !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (n + q + 1)
    setWordArray marr 0 q 0
    nlen <- loop1 marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural nlen narr
  where
    loop1 !marr !i !mem
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr (q + i) ((unsafeShiftL x si) .|. mem)
            loop1 marr (i + 1) (unsafeShiftR x sj)
        | mem /= 0 = do
            writeWordArray marr (q + i) mem
            return (q + i + 1)
        | otherwise = return (q + i)



{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger !(SmallPos 0##) = zeroInteger
negateInteger !(SmallPos !a) = SmallNeg a
negateInteger !(SmallNeg !a) = SmallPos a
negateInteger !(Positive !n !arr) = Negative n arr
negateInteger !(Negative !n !arr) = Positive n arr


{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger !x !y = case (# x, y #) of
    (# SmallPos a, SmallPos b #) -> safeTimesWord Pos a b
    (# SmallPos a, SmallNeg b #) -> safeTimesWord Neg a b
    (# SmallNeg a, SmallPos b #) -> safeTimesWord Neg a b
    (# SmallNeg a, SmallNeg b #) -> safeTimesWord Pos a b

    (# SmallPos 0##, _ #) -> zeroInteger
    (# _, SmallPos 0## #) -> zeroInteger

    (# SmallPos a, Positive n arr #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# a))
    (# SmallPos a, Negative n arr #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# a))
    (# SmallNeg a, Positive n arr #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# a))
    (# SmallNeg a, Negative n arr #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# a))

    (# Positive n arr, SmallPos b #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# b))
    (# Positive n arr, SmallNeg b #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# b))
    (# Negative n arr, SmallPos b #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# b))
    (# Negative n arr, SmallNeg b #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# b))

    (# Positive n1 arr1, Positive n2 arr2 #) -> fromNatural Pos (timesNatural (Natural n1 arr1) (Natural n2 arr2))
    (# Positive n1 arr1, Negative n2 arr2 #) -> fromNatural Neg (timesNatural (Natural n1 arr1) (Natural n2 arr2))

    (# Negative n1 arr1, Positive n2 arr2 #) -> fromNatural Neg (timesNatural (Natural n1 arr1) (Natural n2 arr2))
    (# Negative n1 arr1, Negative n2 arr2 #) -> fromNatural Pos (timesNatural (Natural n1 arr1) (Natural n2 arr2))


{-# INLINE safeTimesWord #-}
safeTimesWord :: Sign -> Word# -> Word# -> Integer
safeTimesWord !sign !w1 !w2 =
    let (# !ovf, !prod #) = timesWord2 (W# w1) (W# w2)
    in case (# ovf == 0, sign #) of
        (# False, Pos #) -> mkPair Positive prod ovf
        (# False, Neg #) -> mkPair Negative prod ovf
        (# True, Pos #) -> SmallPos (unboxWord prod)
        (# True, Neg #) -> SmallNeg (unboxWord prod)

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

{-# INLINE fromSmall #-}
fromSmall :: (Word# -> Integer) -> Word -> Integer
fromSmall !ctor !(W# w#)
    | (W# w#) == 0 = zeroInteger
    | otherwise = ctor w#

{-# INLINE fromNatural #-}
fromNatural :: Sign -> Natural -> Integer
fromNatural !s !(Natural !n !arr)
    | n == 0 = zeroInteger
    | n == 1 && indexWordArray arr 0 == 0 = zeroInteger -- TODO: See if this can be removed.
    | s == Pos = Positive n arr
    | otherwise = Negative n arr

mkPair :: (Int -> WordArray -> Integer) -> Word -> Word -> Integer
mkPair !ctor !lo !hi = runStrictPrim mkNatPair
  where
    mkNatPair :: StrictPrim s Integer
    mkNatPair = do
        marr <- newWordArray 2
        writeWordArray marr 0 lo
        writeWordArray marr 1 hi
        narr <- unsafeFreezeWordArray marr
        return $ ctor 2 narr


zeroInteger, oneInteger, minusOneInteger :: Integer
zeroInteger = SmallPos 0##
oneInteger = SmallPos 1##
minusOneInteger = SmallNeg 1##


toList :: Integer -> [Word]
toList ii =
    case ii of
        SmallPos w -> [W# w]
        SmallNeg w -> [W# w]
        Positive n arr -> natList n arr
        Negative n arr -> natList n arr
  where
    natList n arr = unpackArray 0
        where
            unpackArray i
                | i < n = do
                    let xs = unpackArray (i + 1)
                        x = indexWordArray arr i
                    x : xs
                | otherwise = []

arrayShow :: Int -> WordArray -> String
arrayShow !len !arr =
    let hexify w =
            let x = showHex w ""
            in replicate (16 - length x) '0' ++ x
        digits = dropWhile (== '0') . concatMap hexify . reverse $ unpackArray 0
    in if null digits then "0x0" else "0x" ++ digits
  where
    unpackArray i
        | i < len = do
                let xs = unpackArray (i + 1)
                    x = indexWordArray arr i
                x : xs
        | otherwise = []


signShow :: Sign -> String
signShow Pos = "Pos"
signShow Neg = "Neg"

absInt :: Int -> Int
absInt x = if x < 0 then -x else x

debugWriteWordArray :: Int -> MutableWordArray (StrictPrim s) -> Int -> Word -> StrictPrim s ()
# if 0
debugWriteWordArray line marr i x = do
    debugPrint line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x
#else
debugWriteWordArray _ marr i x = writeWordArray marr i x
#endif

isSmall :: Integer -> Bool
isSmall (SmallPos _) = True
isSmall (SmallNeg _) = True
isSmall _ = False

errorLine :: Int -> String -> a
errorLine linenum s = error $ "Line " ++ show linenum ++ ": " ++ s

isMinimal :: Integer -> Bool
isMinimal i =
    case i of
        SmallPos _ -> True
        SmallNeg a -> isTrue# (neWord# a 0##)
        Positive n arr -> isMinimalNatural n arr
        Negative n arr -> isMinimalNatural n arr
  where
    isMinimalNatural 0 _ = False
    isMinimalNatural n arr = indexWordArray arr (n - 1) /= 0

#endif
