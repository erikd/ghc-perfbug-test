{-# LANGUAGE BangPatterns, CPP #-}
module Check.BenchX where

#define BenchX   1

import qualified NewX.Integer as X

timesBigLoop :: Int -> X.Integer
timesBigLoop iter =
    loop iter count value
  where
    loop :: Int -> Int -> X.Integer -> X.Integer
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count value
    loop !k !j !accum = loop k (j - 1) (X.timesInteger accum value)

    value = X.mkInteger True [ 0x300 .. 0x3ff ]
    count = 10      -- 3 ^ 32 < 0x7fffffffffffffff
