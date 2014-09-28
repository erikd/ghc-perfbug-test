{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module New1.GHC.Integer.Type
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


    , toList, mkLarge

    ) where
-}
    where

import Prelude hiding (Integer, abs, pi, succ) -- (all, error, otherwise, return, show, (++))

import Data.Bits
import Data.Primitive.ByteArray

import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Common.GHC.Integer.Debug
import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import New1.GHC.Integer.Array
import New1.GHC.Integer.Sign

#if !defined(__HADDOCK__)

data Integer
    = Small !Sign
        {-# UNPACK #-} !Word
    | Large !Sign
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !ByteArray

--------------------------------------------------------------------------------

mkInteger :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Integer
mkInteger _ [] = smallInteger 0#
mkInteger True [I# i] = smallInteger i
mkInteger False [I# i] = smallInteger (negateInt# i)
mkInteger nonNegative is =
    let abs = f is
    in if nonNegative
        then abs
        else negateInteger abs
  where
    f [] = smallInteger 0#
    f [I# x] = smallInteger x
    f (I# x : xs) = smallInteger x `orInteger` shiftLInteger (f xs) 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i
    | isTrue# (i ==# 0#) = Small Pos 0
    | isTrue# (i <# 0#) = Small Neg (W# (int2Word# (negateInt# i)))
    | otherwise = Small Pos (W# (int2Word# i))

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = Small Pos (W# w)

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (Small _ (W# w)) = w
integerToWord (Large _ _ arr) = unboxWord (indexWordArray arr 0)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (Small Pos (W# w)) = word2Int# w
integerToInt (Small Neg (W# w)) = negateInt# (word2Int# w)
integerToInt (Large !s _ arr) =
    let i = word2Int# (unboxWord (indexWordArray arr 0))
    in case s of
        Pos -> i
        Neg -> negateInt# i


{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (Small _ 0) b = b
orInteger a (Small _ 0) = a
orInteger (Small Pos a) (Small Pos b) = Small Pos (a .|. b)
orInteger (Small Pos a) (Small Neg b) = Small Neg (1 + (complement a .&. (b - 1)))
orInteger (Small Neg a) (Small Pos b) = Small Neg (1 + ((a - 1) .&. complement b))
orInteger (Small Neg a) (Small Neg b) = Small Neg (1 + ((a - 1) .&. (b - 1)))

orInteger a@(Large _ _ _) b@(Small _ _) = orInteger a (mkLarge b)
orInteger a@(Small _ _) b@(Large _ _ _) = orInteger (mkLarge a) b

orInteger (Large Pos n1 arr1) (Large Pos n2 arr2) = orArray Pos n1 arr1 n2 arr2
orInteger _ _ = error ("New1/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))


orArray :: Sign -> Int -> ByteArray -> Int -> ByteArray -> Integer
orArray !s !n1 !arr1 !n2 !arr2
    | n1 < n2 = orArray s n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        !marr <- newWordArray n1
        !nlen <- loop1 marr 0
        !narr <- unsafeFreezeWordArray marr
        finalizeLarge s nlen narr
  where
    loop1 !marr !i
        | i < n2 = do
                !x <- indexWordArrayM arr1 i
                !y <- indexWordArrayM arr2 i
                writeWordArray marr i (x .|. y)
                loop1 marr (i + 1)
        | otherwise = loop2 marr i
    loop2 !marr !i
        | i < n1 = do
                -- TODO : Use copyArray here?
                !x <- indexWordArrayM arr1 i
                writeWordArray marr i x
                loop2 marr (i + 1)
        | otherwise = return i



{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger a 0# = a
shiftLInteger (Small _ 0) _ = (Small Pos 0)
shiftLInteger a@(Small {}) b = shiftLInteger (mkLarge a) b
shiftLInteger (Large !s !n !arr) b = shiftLArray s n arr (I# b)



{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger (Small !s !a) = Small (negateSign s) a
negateInteger (Large !s !n !arr) = Large (negateSign s) n arr


{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger !x !y = case (# x, y #) of
    (# Small _ _, Small _ 0 #) -> Small Pos 0
    (# Small _ 0, Small _ _ #) -> Small Pos 0
    (# !a, Small Pos 1 #) -> a
    (# Small Pos 1, !b #) -> b

    (# Small !s1 !w1, Small !s2 !w2 #) ->
            safeTimesWord (timesSign s1 s2) w1 w2

    (# Small !s1 !w1, Large !s2 !n2 !arr2 #) ->
            timesArrayW (timesSign s1 s2) n2 arr2 w1

    (# Large !s1 !n1 !arr1, Small !s2 !w2 #) ->
            timesArrayW (timesSign s1 s2) n1 arr1 w2

    (# Large !s1 !n1 !arr1, Large !s2 !n2 !arr2 #) ->
            timesArray (timesSign s1 s2) n1 arr1 n2 arr2

{-# INLINE safeTimesWord #-}
safeTimesWord :: Sign -> Word -> Word -> Integer
safeTimesWord !s !w1 !w2 =
    let (# !ovf, !prod #) = timesWord2 w1 w2
    in if ovf == 0
        then Small s prod
        else mkPair s prod ovf

timesArrayW :: Sign -> Int -> ByteArray -> Word -> Integer
timesArrayW !s !n !arr !w = runStrictPrim $ do
    !marr <- newWordArrayCleared (n + 1)
    writeWordArray marr (n - 1) 0
    loop marr 0 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s (n + 1) narr
  where
    loop !marr !i !carry
        | i < n = do
            !x <- indexWordArrayM arr i
            let (# !c, !p #) = timesWord2C x w carry
            writeWordArray marr i p
            loop marr (i + 1) c
        | otherwise =
            writeWordArray marr i carry


timesArray :: Sign -> Int -> ByteArray -> Int -> ByteArray -> Integer
timesArray !s !n1 !arr1 !n2 !arr2
    | n1 < n2 = timesArray s n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        !psum <- newPlaceholderWordArray
        outerLoop 0 psum 0
  where
    outerLoop !psumLen !psum !s2
        | s2 < n2 = do
            !w <- indexWordArrayM arr2 s2
            if w == 0
                then outerLoop psumLen psum (s2 + 1)
                else do
                    let !newPsumLen = (max psumLen (n1 + s2 + 1)) + 1
                    !marr <- cloneWordArrayExtend psumLen psum newPsumLen
                    !possLen <- innerLoop marr psumLen psum 0 s2 w 0
                    !narr <- unsafeFreezeWordArray marr
                    outerLoop possLen narr (s2 + 1)
        | otherwise =
            finalizeLarge s psumLen psum

    innerLoop !marr !pn !psum !s1 !s2 !hw !carry
        | s1 + s2 < pn && s1 < n1 = do
            !ps <- indexWordArrayM psum (s1 + s2)
            !x <- indexWordArrayM arr1 s1
            let (# !hc, !hp #) = timesWord2CC x hw carry ps
            writeWordArray marr (s1 + s2) hp
            innerLoop marr pn psum (s1 + 1) s2 hw hc
        | s1 < n1 = do
            !x <- indexWordArrayM arr1 s1
            let (# !hc, !hp #) = timesWord2C x hw carry
            writeWordArray marr (s1 + s2) hp
            innerLoop marr pn psum (s1 + 1) s2 hw hc
        | carry /= 0 = do
            writeWordArray marr (s1 + s2) carry
            return (s1 + s2 + 1)
        | otherwise = return (s1 + s2 + 1)


{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

mkLarge :: Integer -> Integer
mkLarge (Small Pos w) = mkSingletonArray Pos w
mkLarge (Small Neg w) = mkSingletonArray Neg w
mkLarge a = a

mkPair :: Sign -> Word -> Word -> Integer
mkPair !sign !lo !hi = runStrictPrim mkLargePair
  where
    mkLargePair :: StrictPrim s Integer
    mkLargePair = do
        !marr <- newWordArray 2
        writeWordArray marr 0 lo
        writeWordArray marr 1 hi
        !narr <- unsafeFreezeWordArray marr
        return $ Large sign 2 narr

mkSingletonArray :: Sign -> Word -> Integer
mkSingletonArray !s !x = runStrictPrim mkSingleton
  where
    mkSingleton :: StrictPrim s Integer
    mkSingleton = do
        !marr <- newWordArray 1
        writeWordArray marr 0 x
        !narr <- unsafeFreezeWordArray marr
        return $ Large s 1 narr

shiftLArray :: Sign -> Int -> ByteArray -> Int -> Integer
shiftLArray !s !n !arr !i
    | i < WORD_SIZE_IN_BITS =
            smallShiftLArray s n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = do
            let (!q, !r) = quotRem i WORD_SIZE_IN_BITS
            if r == 0
                then wordShiftLArray s n arr q
                else largeShiftLArray s n arr (# q, r, WORD_SIZE_IN_BITS - r #)

smallShiftLArray :: Sign -> Int -> ByteArray -> (# Int, Int #) -> Integer
smallShiftLArray !s !n !arr (# !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray (n + 1)
    !nlen <- loop marr 0 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s nlen narr
  where
    loop !marr !i !mem
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftL x si) .|. mem)
            loop marr (i + 1) (unsafeShiftR x sj)
        | mem /= 0 = do
            writeWordArray marr i mem
            return $ i + 1
        | otherwise = return n

-- | TODO : Use copy here
wordShiftLArray :: Sign -> Int -> ByteArray -> Int -> Integer
wordShiftLArray !s !n !arr !q = runStrictPrim $ do
    !marr <- newWordArray (n + q)
    loop1 marr 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s (n + q) narr
  where
    loop1 !marr !i
        | i < q = do
            writeWordArray marr i 0
            loop1 marr (i + 1)
        | otherwise = loop2 marr 0
    loop2 !marr !i
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr (q + i) x
            loop2 marr (i + 1)
        | otherwise = return ()


largeShiftLArray :: Sign -> Int -> ByteArray-> (# Int, Int, Int #) -> Integer
largeShiftLArray !s !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray (n + q + 1)
    setWordArray marr 0 q 0
    loop2 marr 0 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s (n + q + 1) narr
  where
    loop2 !marr !i !mem
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr (q + i) ((unsafeShiftL x si) .|. mem)
            loop2 marr (i + 1) (unsafeShiftR x sj)
        | mem /= 0 = do
            writeWordArray marr (q + i) mem
        | otherwise =
            writeWordArray marr (q + i) 0


shiftRArray :: Sign -> Int -> ByteArray -> Int -> Integer
shiftRArray !s !n !arr !i
    | i < WORD_SIZE_IN_BITS =
            smallShiftRArray s n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = do
            let (!q, !r) = quotRem i WORD_SIZE_IN_BITS
            if q >= n
                then Small Pos 0
                else if r == 0
                    then wordShiftRArray s n arr q
                    else largeShiftRArray s n arr (# q, r, WORD_SIZE_IN_BITS - r #)


smallShiftRArray :: Sign -> Int -> ByteArray -> (# Int, Int #) -> Integer
smallShiftRArray !s !n !arr (# !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray n
    loop marr (n - 1) 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s n narr
  where
    loop !marr !i !mem
        | i >= 0 =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftR x si) .|. mem)
            loop marr (i - 1) (unsafeShiftL x sj)
        | otherwise = return ()

wordShiftRArray :: Sign -> Int -> ByteArray -> Int -> Integer
wordShiftRArray !s !n !arr !q = runStrictPrim $ do
    !marr <- newWordArray (n - q)
    copyWordArray marr 0 arr q (n - q)
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s (n - q) narr


largeShiftRArray :: Sign -> Int -> ByteArray-> (# Int, Int, Int #) -> Integer
largeShiftRArray !s !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray (n - q)
    loop marr (n - q - 1) 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge s (n - q) narr
  where
    loop !marr !i !mem
        | i >= 0 =  do
            !x <- indexWordArrayM arr (q + i)
            writeWordArray marr i ((unsafeShiftR x si) .|. mem)
            loop marr (i - 1) (unsafeShiftL x sj)
        | otherwise = return ()


finalizeLarge :: Sign -> Int -> ByteArray -> StrictPrim s Integer
finalizeLarge !s !nin !arr = do
    let !len = nonZeroLen nin arr
    !x <-indexWordArrayM arr 0
    return $
        if len <= 0 || (len == 1 && x == 0)
            then Small Pos 0
            else if len == 1
                then Small s x
                else Large s len arr

nonZeroLen :: Int -> ByteArray -> Int
nonZeroLen !len !arr
    | len < 1 = 0
    | otherwise =
        let trim i
                | i < 0 = 0
                | indexWordArray arr i == 0 = trim (i - 1)
                | otherwise = i + 1
        in trim (len - 1)


oneInteger, minusOneInteger :: Integer
oneInteger = Small Pos 1
minusOneInteger = Small Neg 1

toList :: Integer -> [Word]
toList (Small Pos w) = [w]
toList (Small Neg w) = [w]
toList (Large _ n arr) =
    unpackArray 0
  where
    unpackArray i
        | i < n = do
                let xs = unpackArray (i + 1)
                    x = indexWordArray arr i
                x : xs
        | otherwise = []

arrayShow :: Int -> ByteArray -> String
arrayShow !len !arr =
    let hexify w =
            let x = showHexW w
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

absInt :: Int -> Int
absInt x = if x < 0 then -x else x


isMinimal :: Integer -> Bool
isMinimal i =
    case i of
        Small _ _ -> True
        Large _ n arr -> indexWordArray arr (n - 1) /= 0


#endif
