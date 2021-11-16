{-# language TypeApplications, AllowAmbiguousTypes, ScopedTypeVariables #-}
module Pure.Hooks.Context (useProvider,useContext,useContext',provide,unprovide,cleanup) where

import Pure.Data.Default (def)
import Pure.Data.View (Comp(..),ask,modify_,get,View)
import Pure.Data.View.Patterns (pattern Component,pattern Null)

import Control.Concurrent.STM (TMVar,newTMVarIO,readTMVar,takeTMVar,putTMVar)
import Control.Monad.STM (atomically)
import Control.Monad (join)
import Data.Foldable (for_)
import Data.Typeable (Typeable,TypeRep,typeOf)
import Data.Unique (Unique,newUnique)
import qualified Data.Map as Map
import GHC.Exts (Any,isTrue#,reallyUnsafePtrEquality#)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

-- Unlike React's Context, this pairing of provide/context is global, not tree-
-- local. To accomplish localization, use newtype wrappers or local coposition.

type Listeners = (Maybe Any,[(Unique,Any -> IO ())])
type Store = TMVar (Map.Map TypeRep Listeners)

{-# NOINLINE store #-}
store :: Store
store = unsafePerformIO (newTMVarIO mempty)

subscribe :: forall a. Typeable a => (a -> IO ()) -> IO (Unique,Maybe a)
subscribe f = do
  let 
    g = f . unsafeCoerce
    tr = typeOf (undefined :: a)
  u <- newUnique
  ma <- join $ atomically $ do
    s <- takeTMVar store
    case Map.lookup tr s of
      Nothing -> do
        let s' = Map.insert tr (Nothing,[(u,g)]) s
        putTMVar store s'
        pure (pure Nothing)
      Just (ma,ls) -> do
        let s' = Map.insert tr (ma,(u,g):ls) s
        putTMVar store s'
        pure (for_ ma g >> pure (unsafeCoerce ma))
  pure (u,ma)

unsubscribe :: forall a. Typeable a => Unique -> IO ()
unsubscribe u = do
  let tr = typeOf (undefined :: a)
  atomically $ do 
    s <- takeTMVar store
    case Map.lookup tr s of
      Just (ma,ls) -> do
        let ls' = filter ((/= u) . fst) ls
        putTMVar store (Map.insert tr (ma,ls') s)
      _ -> 
        putTMVar store s

{-# INLINE provide #-}
provide :: forall a. Typeable a => a -> IO ()
provide a = do
  let 
    any = unsafeCoerce a
    tr = typeOf (undefined :: a)
  join $ atomically $ do
    s <- takeTMVar store
    case Map.lookup tr s of
      Nothing -> do
        let s' = Map.insert tr (Just any,[]) s
        putTMVar store s' 
        pure (pure ())
      Just (ma,ls) -> do
        let s' = Map.insert tr (Just any,ls) s
        putTMVar store s' 
        pure (for_ ls $ \(u,l) -> l any)

{-# INLINE cleanup #-}
cleanup :: forall a. Typeable a => IO ()
cleanup = do
  let
    tr = typeOf (undefined :: a)
  atomically $ do
    s <- takeTMVar store
    putTMVar store (Map.delete tr s)

{-# INLINE unprovide #-}
unprovide :: forall a. Typeable a => IO ()
unprovide = cleanup @a

{-# INLINE useProvider #-}
useProvider :: Typeable a => a -> View -> View
useProvider initial v = flip Component (initial,v) $ \self ->
  let upd = ask self >>= \(v,_) -> provide v
  in def
      { construct = upd
      , update = \_ _ -> upd
      , render = const . snd
      }

{-# INLINE useContext #-}
useContext :: forall a. Typeable a => (a -> View) -> View
useContext = Component $ \self ->
  let upd new = modify_ self (\_ (u,_) -> (u,Just new))
  in def
      { construct = do
        (u,ma) <- subscribe upd
        pure (u,ma)
      , render = \v (_,ma) -> maybe Null v ma
      , unmounted = get self >>= \(u,_) -> unsubscribe @a u
      }

{-# INLINE useContext' #-}
useContext' :: forall a. Typeable a => (Maybe a -> View) -> View
useContext' = Component $ \self ->
  let upd new = modify_ self (\_ (u,_) -> (u,Just new))
  in def
      { construct = do
        (u,ma) <- subscribe upd
        pure (u,ma)
      , render = \v (_,ma) -> v ma
      , unmounted = get self >>= \(u,_) -> unsubscribe @a u
      }