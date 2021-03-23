module Pure.Hooks.Reducer (useReducer,Reducer(..)) where
 
import Pure.Data.Default (def)
import Pure.Data.View (construct,render,modifyM_,View)
import Pure.Data.View.Patterns (pattern Component)

import Data.Typeable (Typeable)

data Reducer cmd a = Reducer 
  { value :: a
  , dispatch :: cmd -> IO ()
  }

{-# INLINE useReducer #-}
useReducer :: (Typeable cmd,Typeable a) => (cmd -> a -> IO a) -> a -> (Reducer cmd a -> View) -> View
useReducer reduce initial = Component $ \self ->
  let 
    upd cmd = modifyM_ self $ \_ (Reducer x k) -> do
      x' <- reduce cmd x
      pure (Reducer x' k,pure ())
  in 
    def
      { construct = pure (Reducer initial upd)
      , render = ($)
      }