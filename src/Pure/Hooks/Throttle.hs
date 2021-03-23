module Pure.Hooks.Throttle (useThrottledEffect,useThrottledEffectWith) where

import Pure.Data.View (View)
import Pure.Data.Time (time,Time)

import Pure.Hooks.Effect (useEffectWith)
import Pure.Hooks.Ref (useRef,Ref(..))

import Control.Monad (when)
import Data.Typeable (Typeable)

{-# INLINE useThrottledEffect #-}
useThrottledEffect :: Time -> IO () -> View -> View
useThrottledEffect d f v = useThrottledEffectWith d f () v

{-# INLINE useThrottledEffectWith #-}
useThrottledEffectWith :: Typeable a => Time -> IO () -> a -> View -> View
useThrottledEffectWith d f deps v = 
  useRef 0 $ \ref ->
    useEffectWith (effect d f ref) (d,f,deps) v
  where 
    effect interval f (Ref getLastRun setLastRun) = do
      ran <- getLastRun
      now <- time
      let since = now - ran
      when (since >= interval) (f >> setLastRun now)
      pure (pure ())