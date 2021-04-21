{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors #-}
#endif
module Distribution.Types.ConfVar (
    ConfVar(..),
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Flag
import Distribution.Types.VersionRange
import Distribution.Compiler
import Distribution.System

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar
instance Structured ConfVar

instance NFData ConfVar where rnf = genericRnf
