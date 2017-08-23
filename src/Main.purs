module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import DOM (DOM)

data Action

type State = {}

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Hello thermite!!"]]

performAction :: T.PerformAction _ State _ Action
performAction _ _ _ = void $ pure unit

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  T.defaultMain spec {} unit
  log "Hello from main"
