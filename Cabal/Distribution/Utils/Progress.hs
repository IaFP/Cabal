{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
-- Note: This module was copied from cabal-install.
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
#endif
-- | A progress monad, which we use to report failure and logging from
-- otherwise pure code.
module Distribution.Utils.Progress
    ( Progress
    , stepProgress
    , failProgress
    , foldProgress
    ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Data.Monoid as Mon
#if MIN_VERSION_base(4,14,0)
import GHC.Types (type (@@), Total)
#endif


-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail (or maybe not expensive, but complicated!)
-- We may get intermediate steps before the final
-- result which may be used to indicate progress and\/or logging messages.
--
-- TODO: Apply Codensity to avoid left-associativity problem.
-- See http://comonad.com/reader/2011/free-monads-for-less/ and
-- http://blog.ezyang.com/2012/01/problem-set-the-codensity-transformation/
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done
  deriving (Functor)
#if MIN_VERSION_base(4,14,0)
instance Total (Progress step fail)
#endif

-- | Emit a step and then continue.
--
stepProgress :: step -> Progress step fail ()
stepProgress step = Step step (Done ())

-- | Fail the computation.
failProgress :: fail -> Progress step fail done
failProgress err = Fail err

-- | Consume a 'Progress' calculation. Much like 'foldr' for lists but with two
-- base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
foldProgress :: (step -> a -> a) -> (fail -> a) -> (done -> a)
             -> Progress step fail done -> a
foldProgress step err done = fold
  where fold (Step s p) = step s (fold p)
        fold (Fail f)   = err f
        fold (Done r)   = done r

instance Monad (Progress step fail) where
  return   = pure
  p >>= f  = foldProgress Step Fail f p

instance Applicative (Progress step fail) where
  pure a  = Done a
  p <*> x = foldProgress Step Fail (flip fmap x) p

instance Monoid fail => Alternative (Progress step fail) where
  empty   = Fail Mon.mempty
  p <|> q = foldProgress Step (const q) Done p
