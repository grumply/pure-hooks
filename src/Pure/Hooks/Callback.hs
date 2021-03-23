module Pure.Hooks.Callback (useCallback,useCallbackWith) where

import Pure.Data.View (View)
import Pure.Hooks.Memo (useMemoWith)

import Data.Typeable ( Typeable )

{-# INLINE useCallback #-}
useCallback :: Typeable a => a -> (a -> View) -> View
useCallback cb v = useCallbackWith cb () v

{-# INLINE useCallbackWith #-}
useCallbackWith :: (Typeable a, Typeable deps) => a -> deps -> (a -> View) -> View
useCallbackWith cb deps v = useMemoWith (\() -> cb) deps $ \f -> v (f ())