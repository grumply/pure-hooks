module Pure.Hooks.Producer (useProducer,useProducerWith,useProducerWith') where

import Pure.Data.Default (def)
import Pure.Data.View (Comp(..),ask,get,View,modify_)
import Pure.Data.View.Patterns (pattern Component,pattern Null)

import Control.Concurrent (forkIO,yield,newEmptyMVar,putMVar,takeMVar)
import Data.Typeable (Typeable)
import Data.IORef (newIORef,readIORef,writeIORef)
import GHC.Exts (isTrue#,reallyUnsafePtrEquality#)

{-
Similar to pure-maybe, but there are no intermediate views - the
component isn't rendered until the provided generator has executed.
-}

{-# INLINE useProducerWith' #-}
useProducerWith' :: (Eq a, Typeable a, Typeable b) => (a -> IO b) -> a -> (b -> View) -> View
useProducerWith' f a v = flip Component (f,a,v) $ \self -> 
  let 
    eff = do
      (f,a,v) <- ask self 
      mv <- newEmptyMVar
      forkIO (f a >>= putMVar mv)
      yield
      b <- takeMVar mv
      pure (v b)
  in 
    def
      { construct = eff
      , mounted   = eff >>= \v -> modify_ self (\_ _ -> v)
      , updated   = \_ _ -> eff >>= \v -> modify_ self (\_ _ -> v)
      , force     = \(_,new,_) _ -> ask self >>= \(_,old,_) -> pure (new /= old)
      , render    = \(_,_,_) -> id
      }

{-# INLINE useProducerWith #-}
useProducerWith :: (Typeable a, Typeable b) => (a -> IO b) -> a -> (b -> View) -> View
useProducerWith f a v = flip Component (f,a,v) $ \self -> 
  let 
    eff = do
      (f,a,v) <- ask self 
      mv <- newEmptyMVar
      forkIO (f a >>= putMVar mv)
      yield
      b <- takeMVar mv
      pure (v b)
  in 
    def
      { construct = eff
      , mounted   = eff >>= \v -> modify_ self (\_ _ -> v)
      , updated   = \_ _ -> eff >>= \v -> modify_ self (\_ _ -> v)
      , force     = \(_,new,_) _ -> ask self >>= \(_,old,_) -> pure (not (isTrue# (reallyUnsafePtrEquality# new old)))
      , render    = \(_,_,_) -> id
      }

{-# INLINE useProducer #-}
useProducer :: Typeable b => IO b -> (b -> View) -> View
useProducer f v = useProducerWith (const f) () v 