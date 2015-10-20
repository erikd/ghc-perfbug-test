{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}

#include "MachDeps.h"

#if __GLASGOW_HASKELL__ < 709
import GHC.Types
#endif

import Numeric (showHex)

import StrictPrim
import Type
import Natural


main :: IO ()
main = do
    putStrLn . hexShowNatural
                $ timesNatural (mkSingletonNat 0x10000) (mkSingletonNat 0x1000)
    checkEtaCount


checkEtaCount :: IO ()
checkEtaCount = do
    text <- readFile "Natural.dump-simpl"
    let etaCount = length $ filter (== "eta") $ words text
    if etaCount > 0
        then error $ "Error : Eta count (" ++ show etaCount ++ ") should be zero."
        else putStrLn "Test passed!"


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

hexShowNatural :: Natural -> String
hexShowNatural (Natural n arr) = arrayShow n arr
