{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


#include "MachDeps.h"

module Integer.Internals where

import Prelude hiding (Integer, abs, pi, sum, rem, succ) -- (all, error, otherwise, return, show, (++))

import Data.Bits

#if __GLASGOW_HASKELL__ < 709
import GHC.Types
#endif

import GHC.Tuple ()

import Numeric (showHex) -- TODO: Remove when its working.

import Integer.Prim
import Integer.StrictPrim
import Integer.Type
import Integer.WordArray


mkNatural :: [Int] -> Natural
mkNatural = f
  where
    f [] = zeroNatural
    f [x] = mkSingletonNat (int2Word x)
    f (x : xs) = (shiftLNatural (f xs) 31) `orNaturalW` (int2Word x)



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


orNaturalW :: Natural -> Word -> Natural
orNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    marr <- newWordArray n
    copyWordArray marr 1 arr 1 (n - 1)
    x <- indexWordArrayM arr 0
    writeWordArray marr 0 (w .|. x)
    narr <- unsafeFreezeWordArray marr
    return $! Natural n narr


shiftLNatural :: Natural -> Int -> Natural
shiftLNatural !nat@(Natural !n !arr) !i
    | i <= 0 = nat
    | i < WORD_SIZE_IN_BITS =
            smallShiftLArray n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = error "shiftLNatural : i >= WORD_SIZE_IN_BITS"

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


{-# INLINE fromNatural #-}
fromNatural :: Sign -> Natural -> Integer
fromNatural !s !(Natural !n !arr)
    | n == 0 = zeroInteger
    | n == 1 && indexWordArray arr 0 == 0 = zeroInteger -- TODO: See if this can be removed.
    | s == Pos = Positive n arr
    | otherwise = Negative n arr

zeroInteger :: Integer
zeroInteger = SmallPos 0##


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


mkSingletonNat :: Word -> Natural
mkSingletonNat !x = runStrictPrim mkNat
  where
    mkNat :: StrictPrim s Natural
    mkNat = do
        marr <- newWordArray 1
        writeWordArray marr 0 x
        narr <- unsafeFreezeWordArray marr
        return $ Natural 1 narr


finalizeNatural :: Int -> WordArray -> StrictPrim s Natural
finalizeNatural 0 _ = return zeroNatural
finalizeNatural !nin !arr = do
    let !len = nonZeroLen nin arr
    x <- indexWordArrayM arr 0
    return $
        if len < 0 || (len == 1 && x == 0)
            then zeroNatural
            else Natural len arr

nonZeroLen :: Int -> WordArray -> Int
nonZeroLen !len !arr
    | len < 1 = 0
    | otherwise =
        let trim i
                | i < 0 = 0
                | indexWordArray arr i == 0 = trim (i - 1)
                | otherwise = i + 1
        in trim (len - 1)


zeroNatural :: Natural
zeroNatural = runStrictPrim $ do
        marr <- newWordArray 1
        writeWordArray marr 0 0
        narr <- unsafeFreezeWordArray marr
        return $! Natural 0 narr

hexShowNatural :: Natural -> String
hexShowNatural (Natural n arr) = arrayShow n arr
