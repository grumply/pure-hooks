module Pure.Hooks.State (State,state,modify,get,put) where

import Pure.Elm.Fold hiding (modify,get)

import Data.Typeable

{-
This is a typed wrapper around `fold ($)` with a convenience constraint: `State`.
-}

data Get a = Get a
newtype Modify a = Modify (a -> a)

type State a = (Elm (Modify a),Has (Get a))

modify :: Elm (Modify a) => (a -> a) -> IO ()
modify = command . Modify

put :: Elm (Modify a) => a -> IO ()
put = modify . const

get :: Has (Get a) => a
get = let Get a = it in a 

{-# INLINE state #-}
state :: Typeable a => a -> (State a => View) -> View
state initial v = fold (\(Modify f) (Get a) -> Get (f a)) (Get initial) v
