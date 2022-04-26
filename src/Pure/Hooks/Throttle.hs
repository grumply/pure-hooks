module Pure.Hooks.Throttle (useThrottledEffect,useThrottledEffectWith) where

import Pure.Data.View (View)
import Pure.Data.Time (time,Time)

import Pure.Hooks.Effect (useEffectWith)
import Pure.Hooks.Ref (ref,deref,assign,Ref)

import Control.Monad (when)
import Data.Typeable (Typeable)

{-# INLINE useThrottledEffect #-}
useThrottledEffect :: Time -> IO () -> View -> View
useThrottledEffect d f v = useThrottledEffectWith d f () v

{-# INLINE useThrottledEffectWith #-}
useThrottledEffectWith :: Typeable a => Time -> IO () -> a -> View -> View
useThrottledEffectWith d f deps v = 
  ref (0 :: Time) $
    useEffectWith (effect d f) (d,f,deps) v
  where 
    effect :: Ref Time => Time -> IO () -> IO (IO ())
    effect interval f = do
      ran <- deref
      now <- time
      let since = now - ran
      when (since >= interval) (f >> assign now)
      pure (pure ())
