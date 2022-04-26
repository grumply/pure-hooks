module Pure.Hooks.Reducer (reducer,reduced,Reducer) where

import Pure.Elm.Fold

import Data.Typeable (Typeable)

data Command command = Command command
data Reduced state = Reduced state

type Reducer command state = (Elm (Command command),Has (Reduced state))

reduced :: Has (Reduced state) => state
reduced = let Reduced state = it in state

reducer :: (Typeable command, Typeable state) => (command -> state -> IO state) -> state -> (Reducer command state => View) -> View
reducer f initial = foldM update (pure (Reduced initial,pure ()))
  where
    update (Command command) (Reduced state) = do
      new <- f command state
      pure (Reduced new)

