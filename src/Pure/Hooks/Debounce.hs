module Pure.Hooks.Debounce (useDebouncedEffect,useDebouncedEffectWith) where

import Pure.Data.View (View)
import Pure.Data.Time (delay,Time)

import Pure.Hooks.Effect (useEffectWith)
import Pure.Hooks.Ref (useRef,Ref(..))

import Control.Concurrent (forkIO,killThread)
import Data.Typeable (Typeable)

{-# INLINE useDebouncedEffect #-}
useDebouncedEffect :: Time -> IO () -> View -> View
useDebouncedEffect d f v = useDebouncedEffectWith d f () v

{-# INLINE useDebouncedEffectWith #-}
useDebouncedEffectWith :: Typeable a => Time -> IO () -> a -> View -> View
useDebouncedEffectWith d f deps v = 
  useRef False $ \ref ->
    useEffectWith (effect d f ref) (d,f,deps) v
  where 
    effect d f (Ref getRan setRan) = do
      ran <- getRan
      if ran then do
        tid <- forkIO $ do
          delay d
          f
        pure (killThread tid)
      else do
        f
        setRan True
        pure (pure ())