
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  New1.GHC.Integer
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

module New1.GHC.Integer (
    Integer, mkInteger,
    smallInteger, wordToInteger,

    timesInteger,

    orInteger,
    shiftLInteger
    ) where

import New1.GHC.Integer.Type

