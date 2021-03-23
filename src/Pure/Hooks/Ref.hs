module Pure.Hooks.Ref where

import Pure.Data.Default (def)
import Pure.Data.View (construct,render,modify_,View)
import Pure.Data.View.Patterns (pattern Component)

import Data.Typeable (Typeable)
import Data.IORef (newIORef,readIORef,writeIORef)

data Ref a = Ref 
  { deref :: IO a
  , assign :: a -> IO ()
  }

{-# INLINE useRef #-}
useRef :: Typeable a => a -> (Ref a -> View) -> View
useRef initial = Component $ \self ->
  def
    { construct = do
      ref <- newIORef initial 
      pure (Ref (readIORef ref) (writeIORef ref))
    , render = ($)
    } 

