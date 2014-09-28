{-# LANGUAGE CPP,  NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  New3.GHC.Integer
-- Copyright   :  (c) Erik de Castro Lopo
-- License     :  BSD3
--
-- Maintainer  :  <erikd@mega-nerd.com>
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- An simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module New3.GHC.Integer (
    Integer, mkInteger,
    smallInteger, wordToInteger,

    timesInteger,

    shiftLInteger,

    -- Testing only.
    isMinimal
    ) where

import New3.GHC.Integer.Internals
import New3.GHC.Integer.Type
