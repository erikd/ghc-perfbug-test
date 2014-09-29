{-# LANGUAGE CPP, MagicHash, ScopedTypeVariables #-}

import Prelude hiding (Integer)

import qualified Criterion.Main as C

import Check.Bench1 as Bench1
import Check.Bench3 as Bench3


main :: IO ()
main = C.defaultMain [ timesBigBench 10 ]


timesBigBench :: Int -> C.Benchmark
timesBigBench loopCount =
    C.bgroup "Big Integer multiplication"
            [ C.bench "New1-A"    $ C.whnf Bench1.timesBigLoop loopCount
            , C.bench "New3-A"    $ C.whnf Bench3.timesBigLoop loopCount
            , C.bench "New1-B"    $ C.whnf Bench1.timesBigLoop loopCount
            , C.bench "New3-B"    $ C.whnf Bench3.timesBigLoop loopCount
            , C.bench "New1-C"    $ C.whnf Bench1.timesBigLoop loopCount
            , C.bench "New3-C"    $ C.whnf Bench3.timesBigLoop loopCount
            ]
