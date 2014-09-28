{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New3.Integer
    ( module New3.GHC.Integer
    , hexShow
    , hexShowNatural
    ) where

import Prelude hiding (Integer)
import Numeric

import Common.GHC.Integer.Prim
import New3.GHC.Integer
import New3.GHC.Integer.Internals
import New3.GHC.Integer.Type


instance Num Integer where
    (+) = error "New3.Integer: plusInteger"
    (-) = error "New3.Integer: minusInteger"
    (*) = timesInteger
    abs = error "New3.Integer: absInteger"
    signum = error "New3.Integer: signumInteger"
    fromInteger = error "New3.Integer: fromInteger"


instance Show Integer where
    show = hexShow

instance Show Natural where
    show = hexShowNatural


hexShow :: Integer -> String
hexShow (SmallPos 0##) = "0x0"
hexShow (SmallPos a) = "+0x" ++ showHex (boxWord# a) ""
hexShow (SmallNeg a) = "-0x" ++ showHex (boxWord# a) ""
hexShow (Positive n arr) = '+' : hexShowNatural (Natural n arr)
hexShow (Negative n arr) = '-' : hexShowNatural (Natural n arr)

hexShowNatural :: Natural -> String
hexShowNatural (Natural n arr) = arrayShow n arr

