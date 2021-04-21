{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.DList
-- Copyright   :  (c) Ben Gamari 2015-2019
-- License     :  BSD3
--
-- Maintainer  :  cabal-dev@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A very simple difference list.
module Distribution.Compat.DList (
    DList,
    runDList,
    singleton,
    fromList,
    toList,
    snoc,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (toList)
#if MIN_VERSION_base(4,14,0)
import GHC.Types (Total, type (@@))
#endif

-- | Difference list.
newtype DList a = DList ([a] -> [a])

runDList :: DList a -> [a]
runDList (DList run) = run []

-- | Make 'DList' with containing single element.
singleton :: a -> DList a
singleton a = DList (a:)

fromList :: [a] -> DList a
fromList as = DList (as ++)

toList :: DList a -> [a]
toList = runDList

snoc :: DList a -> a -> DList a
snoc xs x = xs <> singleton x

instance Monoid (DList a) where
  mempty = DList id
  mappend = (<>)

instance Semigroup (DList a) where
  DList a <> DList b = DList (a . b)
