module Pure.Hooks.Ref (Ref,ref,deref,assign) where

import Pure.Elm.Fold hiding (Ref)

import Data.Typeable (Typeable)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)

{-
A wrapper around `foldM writeIORef (newIORef a,pure ())` with a convenience constraint: `Ref`.
-}

newtype Assign a = Assign a
newtype Reference a = Reference (IORef a)

type Ref a = (Elm (Assign a), Has (Reference a))

deref :: Has (Reference a) => IO a
deref = let Reference r = it in readIORef r

assign :: Elm (Assign a) => a -> IO ()
assign = command . Assign

ref :: Typeable a => a -> (Ref a => View) -> View
ref initial f = foldM (\(Assign a) (Reference r) -> writeIORef r a >> pure (Reference r)) start f
  where
    start = do
      r <- newIORef initial
      pure (Reference r,pure ())
