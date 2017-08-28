module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import React (ReactElement, createFactory)
import React.DOM as R
import ReactDOM as RD
import Thermite as T

data Action

type Story = { title :: String }

type State = { topStories :: Array Story }

genFakeStory :: Int -> Story
genFakeStory idx = { title: "Fake Story Title " <> show idx }

initState :: State
initState = { topStories: map genFakeStory (1 .. 30)}

renderStoryItem :: Story -> ReactElement
renderStoryItem { title } = R.div' [ R.text title ]

renderStoryList :: Array Story -> Array ReactElement
renderStoryList = map renderStoryItem

render :: forall eff. T.Render State eff Action
render dispatch _ state _ =
  [ R.p' [ R.text "Hello thermite!!" ]
  , R.div' $ renderStoryList state.topStories
  ]

spec :: forall eff. T.Spec eff State Unit Action
spec = T.simpleSpec T.defaultPerformAction render

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  htmlDocument <- DOM.window >>= DOM.document
  let document = htmlDocumentToDocument htmlDocument
  maybeContainer <- getElementById (ElementId "app") $ documentToNonElementParentNode document
  case maybeContainer of
    Just container -> do
      let component = T.createClass spec initState
      let reactElement = createFactory component unit
      void $ RD.render reactElement container
    Nothing -> log "container #app not found"
