{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
#endif
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Verbosity.Internal
  ( VerbosityLevel(..)
  , VerbosityFlag(..)
  ) where

import Prelude ()
import Distribution.Compat.Prelude

data VerbosityLevel = Silent | Normal | Verbose | Deafening
    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable)

instance Binary VerbosityLevel
instance Structured VerbosityLevel

data VerbosityFlag
    = VCallStack
    | VCallSite
    | VNoWrap
    | VMarkOutput
    | VTimestamp
    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable)

instance Binary VerbosityFlag
instance Structured VerbosityFlag
