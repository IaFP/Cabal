{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif

module Distribution.Utils.MapAccum (mapAccumM) where

import Distribution.Compat.Prelude
import Prelude ()
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type(@))
#endif  
-- Like StateT but with return tuple swapped
newtype
#if MIN_VERSION_base(4,16,0)
  m @ (s, a) =>
#endif
  StateM s m a = StateM { runStateM :: s -> m (s, a) }

instance Functor m => Functor (StateM s m) where
    fmap f (StateM x) = StateM $ \s -> fmap (\(s', a) -> (s', f a)) (x s)

instance
#if __GLASGOW_HASKELL__ < 709
    (Functor m, Monad m)
#else
    (
#if __GLASGOW_HASKELL__ >= 903
     Applicative m, 
#endif
     Monad m)
#endif
    => Applicative (StateM s m) where
    pure x = StateM $ \s -> return (s, x)
    StateM f <*> StateM x = StateM $ \s -> do (s', f') <- f s
                                              (s'', x') <- x s'
                                              return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM ::
#if __GLASGOW_HASKELL__ < 709
    (Functor m, Monad m, Traversable t)
#else
    (
#if __GLASGOW_HASKELL__ >= 903
     Applicative m, t @ StateM a m c,
#endif
     Monad m, Traversable t)
#endif
          => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (\s' -> f s' x)) t) s

