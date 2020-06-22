module Concur.React where

import Prelude

import Concur.Core.Discharge (discharge, dischargePartialEffect)
import Concur.Core.Types (Widget)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import React.Basic as R

type HTML
  = Array R.JSX

-- React apparently requires wrapping state inside an object
type ComponentState
  = { view :: HTML }

mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

componentClassWithMount :: forall a. Effect Unit -> Widget HTML a -> R.Component {}
componentClassWithMount onMount winit
  = do
    Tuple winit' v <- dischargePartialEffect winit
    R.make (R.createComponent "Concur")
      { initialState: mkComponentState v
      , render: \self -> render <$> self.state
      , componentDidMount: \self -> onMount *> handler self (Right winit')
      }
    where
      handler self (Right r) = do
        v <- discharge (handler self) r
        void $ self.setState (mkComponentState v)
      handler _ (Left err) = do
        log ("FAILED! " <> show err)
        pure unit
      render st = R.element st.view

componentClass :: forall a. Widget HTML a -> R.Component {}
componentClass = componentClassWithMount mempty

renderComponent :: forall a. Widget HTML a -> R.JSX
renderComponent init = R.element (componentClass init) {}
