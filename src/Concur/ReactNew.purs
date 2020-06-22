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

type WidgetProps = {}

mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

componentClassWithMount :: forall a. Effect Unit -> Widget HTML a -> WidgetProps -> R.JSX
componentClassWithMount onMount winit = component
  where
    component this = do
      Tuple winit' v <- dischargePartialEffect winit
      pure $ R.make (R.createComponent "Concur")
        { initialState: mkComponentState v
        , render: R.element this.state.view
        , componentDidMount: onMount *> handler this (Right winit')
        }
    handler self (Right r) = do
      v <- discharge (handler self) r
      void $ self.setState (mkComponentState v)
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit

componentClass :: forall a. Widget HTML a -> R.Component {}
componentClass = componentClassWithMount mempty

renderComponent :: forall a. Widget HTML a -> R.JSX
renderComponent init = componentClass init {}
