{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}
#endif

module Distribution.Utils.MapAccum (mapAccumM) where

import Distribution.Compat.Prelude
import Prelude ()
#if MIN_VERSION_base(4,14,0)
import GHC.Types (Total, type (@@))
#endif

-- Like StateT but with return tuple swapped
data StateM s m a = StateM { runStateM :: s -> m (s, a) }
#if MIN_VERSION_base(4,14,0)
instance Total (StateM s m)
#endif

instance Functor m => Functor (StateM s m) where
    fmap f (StateM x) = StateM $ \s -> fmap (\(s', a) -> (s', f a)) (x s)

instance
#if __GLASGOW_HASKELL__ < 709
    (Functor m, Monad m
#else
    (Monad m
#if MIN_VERSION_base(4,14,0)
    , Total m
#endif
#endif
    )=> Applicative (StateM s m) where
    pure x = StateM $ \s -> return (s, x)
    StateM f <*> StateM x = StateM $ \s -> do (s', f') <- f s
                                              (s'', x') <- x s'
                                              return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM ::
#if __GLASGOW_HASKELL__ < 709
    (Functor m, Monad m, Traversable t)
#else
    (Monad m, Traversable t
#if MIN_VERSION_base(4,14,0)
    , t @@ StateM a m c, Total m
#endif
    )
#endif
          => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (\s' -> f s' x)) t) s

