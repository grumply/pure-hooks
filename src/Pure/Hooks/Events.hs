{-# language TypeApplications, AllowAmbiguousTypes, ScopedTypeVariables #-}
module Pure.Hooks.Events (useEvents,event) where

import Pure.Data.Default (def)
import Pure.Data.View (Comp(..),ask,modifyM_,get,View)
import Pure.Data.View.Patterns (pattern Component)

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

-- This implementation is still a little wonky. It needs a fleshed-out purpose
-- before I can really understand what a reasonable implementation would be.

type Listeners = [(Unique,Any -> IO ())]
type Broker = TMVar (Map.Map TypeRep Listeners)

{-# NOINLINE broker #-}
broker :: Broker
broker = unsafePerformIO (newTMVarIO mempty)

subscribe :: forall ev. Typeable ev => (ev -> IO ()) -> IO Unique
subscribe f = do
  let 
    g = f . unsafeCoerce
    tr = typeOf (undefined :: ev)
  u <- newUnique
  atomically $ do
    s <- takeTMVar broker
    case Map.lookup tr s of
      Nothing -> do
        let s' = Map.insert tr [(u,g)] s
        putTMVar broker s'
      Just ls -> do
        let s' = Map.insert tr ((u,g):ls) s
        putTMVar broker s'
  pure u

unsubscribe :: forall ev. Typeable ev => Unique -> IO ()
unsubscribe u = do
  let tr = typeOf (undefined :: ev)
  atomically $ do 
    s <- takeTMVar broker
    case Map.lookup tr s of
      Just ls -> do
        let ls' = filter ((/= u) . fst) ls
        putTMVar broker (Map.insert tr ls' s)
      _ -> 
        putTMVar broker s

{-# INLINE event #-}
event :: forall ev. Typeable ev => ev -> IO ()
event ev = do
  let 
    any = unsafeCoerce ev
    tr = typeOf (undefined :: ev)
  join $ atomically $ do
    s <- readTMVar broker
    let mls = Map.lookup tr s
    pure $ 
      for_ mls $ \ls ->
        for_ ls $ \(_,l) -> 
          l any

{-# INLINE useEvents #-}
useEvents :: forall ev. Typeable ev => (ev -> IO ()) -> View -> View
useEvents callback v = flip Component (callback,v) $ \self ->
  let upd ev = modifyM_ self $ \(callback,_) u -> callback ev >> pure (u,pure ())
  in def
      { construct = subscribe @ev upd
      , render = \(_,v) _ -> v
      , unmounted = get self >>= unsubscribe @ev
      }