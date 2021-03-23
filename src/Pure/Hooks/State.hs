{-# language ScopedTypeVariables #-}
module Pure.Hooks.State (useState,State(..)) where

import Pure.Data.Default (def)
import Pure.Data.View (construct,render,modify_,View)
import Pure.Data.View.Patterns (pattern Component)

import Data.Typeable (Typeable)

data State a = State 
  { state :: a
  , modify :: (a -> a) -> IO ()
  }

{-# INLINE useState #-}
useState :: forall a. Typeable a => a -> (State a -> View) -> View
useState initial = Component $ \self ->
  let upd f = modify_ self $ \_ (~(State x k)) -> State (f x) k
  in def
      { construct = pure (State initial upd)
      , render = ($)
      }