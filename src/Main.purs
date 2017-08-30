module Main where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Argonaut (decodeJson)
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Ajax
import React (ReactElement, createFactory)
import React as React
import React.DOM as R
import ReactDOM as RD
import Thermite as T

data Action = RootDidMount

type Story = { title :: String }

type State = { topStories :: Array Story
             , topStoryIds :: Array Int }

genFakeStory :: Int -> Story
genFakeStory idx = { title: "Fake Story Title " <> show idx }

initState :: State
initState = { topStories: map genFakeStory (1 .. 30)
            , topStoryIds: []
            }

renderStoryItem :: Story -> ReactElement
renderStoryItem { title } = R.div' [ R.text title ]

renderStoryList :: Array Story -> Array ReactElement
renderStoryList = map renderStoryItem

performAction :: forall eff props.
                 T.PerformAction (ajax :: AJAX, console :: CONSOLE | eff) State props Action
performAction RootDidMount props state = do
  eitherRes <- getTopStories
  let eitherIds = eitherRes >>= (_.response >>> decodeJson >>> lmap error)
  lift $ logShow eitherIds
  void $ T.modifyState \s -> s { topStoryIds = either (const []) id eitherIds }

  where getTopStories =
          lift $ attempt $ Ajax.get "https://hacker-news.firebaseio.com/v0/topstories.json"

render :: forall eff. T.Render State eff Action
render dispatch _ state _ =
  [ R.p' [ R.text "Hello thermite!!" ]
  , R.div' $ renderStoryList <<< map genFakeStory $ state.topStoryIds
  ]

spec :: forall eff. T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State Unit Action
spec = T.simpleSpec performAction render

type ReactSpec props state eff =
  { spec :: React.ReactSpec props state eff
  , dispatcher :: React.ReactThis props state -> Action -> T.EventHandler
  }

withComponentDidMount :: forall props state eff
                       . ReactSpec props state eff
                      -> ReactSpec props state eff
withComponentDidMount rs = rs { spec = specWithMount }
  where
    specWithMount = rs.spec { componentDidMount = handleDidMount }
    handleDidMount this = rs.dispatcher this RootDidMount

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  htmlDocument <- DOM.window >>= DOM.document
  let document = htmlDocumentToDocument htmlDocument
  maybeContainer <- getElementById (ElementId "app") $ documentToNonElementParentNode document
  case maybeContainer of
    Just container -> do
      let
        -- TODO the purescript react seems missing the type annotation for extensible effects
        reactSpec = T.createReactSpec spec initState
        component = React.createClass <<< _.spec $ withComponentDidMount reactSpec
        reactElement = createFactory component unit
      void $ RD.render reactElement container
    Nothing -> Eff.log "container #app not found"
