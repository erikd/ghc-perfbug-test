{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module Integer.Type where

import GHC.Classes
import GHC.Prim
import GHC.Types


import Integer.WordArray

data Sign
    = Pos | Neg
    deriving Eq

data Integer
    = SmallPos Word#
    | SmallNeg Word#
    | Positive
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !WordArray
    | Negative
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !WordArray

data Natural
    = Natural
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !WordArray
