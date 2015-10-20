{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

#include "MachDeps.h"

module Type where

import Prelude hiding (sum, ($!))

import GHC.Prim
import GHC.Types
import Control.Monad.Primitive
import Data.Primitive


data Natural
    = Natural {-# UNPACK #-} !Int {-# UNPACK #-} !WordArray

newtype WordArray = WA ByteArray

newtype MutableWordArray m = MWA (MutableByteArray (PrimState m))

{-# INLINE newWordArray #-}
newWordArray :: (Monad m, PrimMonad m) => Int -> m (MutableWordArray m)
newWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    return $ MWA marr

{-# INLINE unsafeFreezeWordArray #-}
unsafeFreezeWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> m WordArray
unsafeFreezeWordArray !(MWA !marr) = do
    !arr <- unsafeFreezeByteArray marr
    return (WA arr)

{-# INLINE indexWordArray #-}
indexWordArray :: WordArray -> Int -> Word
indexWordArray !(WA !arr) = indexByteArray arr

{-# INLINE indexWordArrayM #-}
indexWordArrayM :: Monad m => WordArray -> Int -> m Word
indexWordArrayM !(WA !arr) !i = case indexByteArray arr i of x -> return x

{-# INLINE writeWordArray #-}
writeWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> Int -> Word -> m ()
writeWordArray !(MWA !marr) = writeByteArray marr

{-# INLINE plusWord #-}
plusWord :: Word -> Word -> Word
plusWord (W# a) (W# b) =
    let !s = plusWord# a b
    in W# s

{-# INLINE plusWord2 #-}
plusWord2 :: Word -> Word -> (# Word, Word #)
plusWord2 (W# a) (W# b) =
    let (# !c, !s #) = plusWord2# a b
    in (# W# c, W# s #)

{-# INLINE plusWord2C #-}
plusWord2C :: Word -> Word -> Word -> (# Word, Word #)
plusWord2C (W# a) (W# b) (W# c) =
    let (# !c1, !s1 #) = plusWord2# a b
        (# !c2, !s2 #) = plusWord2# s1 c
        !carry = plusWord# c1 c2
    in (# W# carry, W# s2 #)

{-# INLINE timesWord2 #-}
timesWord2 :: Word -> Word -> (# Word, Word #)
timesWord2 (W# a) (W# b) =
    let (# !ovf, !prod #) = timesWord2# a b
    in (# W# ovf, W# prod #)

