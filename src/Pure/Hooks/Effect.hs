module Pure.Hooks.Effect (useEffect,useEffectWith,useEffectWith') where

import Pure.Data.Default (def)
import Pure.Data.View (Comp(..),ask,get,View)
import Pure.Data.View.Patterns (pattern Component)

import Control.Monad (join)
import Data.Typeable (Typeable)
import Data.IORef (newIORef,readIORef,writeIORef)
import GHC.Exts (isTrue#,reallyUnsafePtrEquality#)

{-

Unlike most hooks, `effect` cannot be implemented with `Pure.Elm.Fold` because
only the view of `fold` is dynamic.

-}

{-# INLINE useEffectWith' #-}
useEffectWith' :: (Eq a, Typeable a) => IO (IO ()) -> a -> View -> View
useEffectWith' f deps v = flip Component (f,deps,v) $ \self -> 
  let 
    eff = do
      (f,_,_)  <- ask self 
      cleanup_ <- get self 
      join (readIORef cleanup_)
      cleanup <- f
      writeIORef cleanup_ cleanup
  in 
    def
      { construct = newIORef (pure ())
      , mounted   = eff
      , updated   = \_ _ -> eff
      , force     = \(_,new,_) _ -> ask self >>= \(_,old,_) -> pure (new /= old)
      , unmounted = get self >>= join . readIORef
      , render    = \(_,_,v) _ -> v
      }

{-# INLINE useEffectWith #-}
useEffectWith :: Typeable a => IO (IO ()) -> a -> View -> View
useEffectWith f deps v = flip Component (f,deps,v) $ \self -> 
  let 
    eff = do
      (f,_,_)  <- ask self 
      cleanup_ <- get self 
      join (readIORef cleanup_)
      cleanup <- f
      writeIORef cleanup_ cleanup
  in 
    def
      { construct = newIORef (pure ())
      , mounted   = eff
      , updated   = \_ _ -> eff
      , force     = \(_,new,_) _ -> ask self >>= \(_,old,_) -> pure (not (isTrue# (reallyUnsafePtrEquality# new old)))
      , unmounted = get self >>= join . readIORef
      , render    = \(_,_,v) _ -> v
      }

{-# INLINE useEffect #-}
useEffect :: IO (IO ()) -> View -> View
useEffect f v = useEffectWith f () v
