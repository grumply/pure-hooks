module Pure.Hooks.Memo (useMemo,useMemoWith) where

import Pure.Data.Default (def)
import Pure.Data.View (Comp(..),modify_,ask,get,View)
import Pure.Data.View.Patterns (pattern Component)

import Control.Monad (join)
import Data.Typeable (Typeable)
import Data.IORef (newIORef,readIORef,writeIORef)
import GHC.Exts (isTrue#,reallyUnsafePtrEquality#)

{-# INLINE useMemo #-}
useMemo :: Typeable a => a -> (a -> View) -> View
useMemo a v = useMemoWith a () v

{-# INLINE useMemoWith #-}
useMemoWith :: (Typeable a, Typeable deps) => a -> deps -> (a -> View) -> View
useMemoWith f deps v = flip Component (\() -> f,deps,v) $ \self ->
  let upd = modify_ self $ \(f,_,_) _ -> f ()
  in def
      { construct = ask self >>= \(f,_,_) -> pure (f ())
      , updated   = \_ _ -> upd
      , force     = \(_,new,_) _ -> ask self >>= \(_,old,_) -> pure (not (isTrue# (reallyUnsafePtrEquality# new old)))
      , render    = \(_,_,v) -> v
      }

