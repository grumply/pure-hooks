module Pure.Hooks.Debounce (useDebouncedEffect,useDebouncedEffectWith) where

import Pure.Data.View (View)
import Pure.Data.Time (delay,Time)

import Pure.Hooks.Effect (useEffectWith)
import Pure.Hooks.Ref (Ref,ref,assign,deref)

import Control.Concurrent (forkIO,killThread)
import Data.Typeable (Typeable)

{-# INLINE useDebouncedEffect #-}
useDebouncedEffect :: Time -> IO () -> View -> View
useDebouncedEffect d f v = useDebouncedEffectWith d f () v

{-# INLINE useDebouncedEffectWith #-}
useDebouncedEffectWith :: Typeable a => Time -> IO () -> a -> View -> View
useDebouncedEffectWith d f deps v = 
  ref False $ 
    useEffectWith (effect d f) (d,f,deps) v
  where 
    effect :: Ref Bool => Time -> IO () -> IO (IO ())
    effect d f = do
      ran <- deref
      if ran then do
        tid <- forkIO $ do
          delay d
          f
        pure (killThread tid)
      else do
        f
        assign True
        pure (pure ())
