{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import Test.Hspec

import qualified Check.New3 as New3


main :: IO ()
main = hspec $ do
    describe "Comparing New1 and New3 Integer operations:" New3.testNewInteger

