module Main where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Except.Trans (class MonadTrans)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data (Story(..), State)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Array (fold, singleton)
import Data.Either (either)
import Data.Lens (Lens', Prism', lens, over, prism')
import Data.List (List, catMaybes, take)
import Data.Maybe (Maybe(..))
import Data.Monoid as L
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Traversable (for, for_, traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Ajax
import React (createFactory)
import React as React
import React.DOM as R
import ReactDOM as RD
import Thermite (Render)
import Thermite as T

data StoryAction

data Action = RootDidMount
            | StoryAction Int StoryAction

initState :: State
initState = { topStories: M.empty
            , topStoryIds: L.mempty
            }

_topStoryIds :: Lens' State (List Int)
_topStoryIds = lens _.topStoryIds (_ { topStoryIds = _})

_topStoryList :: Lens' State (List Story)
_topStoryList = lens toList mergeState
  where
    toList :: State -> List Story
    toList st = catMaybes $ map (\id -> M.lookup (show id) st.topStories) st.topStoryIds

    mergeState :: State -> List Story -> State
    mergeState = const -- TODO this may useful for normalize

_StoryAction :: Prism' Action (Tuple Int StoryAction)
_StoryAction = prism' (uncurry StoryAction) \a ->
  case a of
    (StoryAction idx sa)-> Just (Tuple idx sa)
    _ -> Nothing

storyItemSpec :: forall eff. T.Spec eff Story Unit StoryAction
storyItemSpec = T.simpleSpec T.defaultPerformAction render
  where
    render :: Render Story Unit StoryAction
    render dispatch _ (Story st) _ = singleton $ R.div' [ R.text $ st.title ]

parseOrThrow :: forall m a. MonadThrow Error m => DecodeJson a => Json -> m a
parseOrThrow json = either (throwError <<< error) pure $ decodeJson json

performAction :: forall eff props.
                 T.PerformAction (ajax :: AJAX, console :: CONSOLE | eff) State props Action
performAction RootDidMount props state = do
  result <- runExceptT do
    res <- liftAff $ Ajax.get "https://hacker-news.firebaseio.com/v0/topstories.json"
    ids <- liftAff $ parseOrThrow res.response
    _ <- lift $ T.modifyState \s -> s { topStoryIds = ids }
    (stories :: List Story) <- liftAff $ sequential $ for (take 30 ids) $ \id -> parallel $ do
      itemRes <- Ajax.get $ "https://hacker-news.firebaseio.com/v0/item/" <> show id <> ".json"
      parseOrThrow itemRes.response

    liftAff $ traverse_ (log <<< _.title <<< unwrap) stories
    lift $ for_ stories \story ->
      T.modifyState \s ->
        s { topStories = M.insert (show <<< _.id <<< unwrap $ story) story s.topStories }
    pure unit

  either
    (\e -> lift $ log $ "Caught error: \n" <> show e)
    (\r -> pure unit)
    result

  where
    liftAff :: forall eff1 m a
             . MonadTrans m
            => Aff eff1 a
            -> ExceptT Error (m (Aff eff1)) a
    liftAff = ExceptT <<< lift <<< attempt

  -- eitherIds <- lift $ attempt $ do
  --   res <- Ajax.get "https://hacker-news.firebaseio.com/v0/topstories.json"
  --   parseOrThrow res.response
  -- case eitherIds of
  --   Right ids -> void $ T.modifyState \s -> s { topStoryIds = ids }
  --   Left error -> lift $ logShow error

performAction _ _ _ = pure unit

spec :: forall eff. T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State Unit Action
spec = container $ fold
  [ stories
  , actions
  ]
  where
    container = over T._render \render d p s c ->
      [ R.p' [ R.text "Hello thermite!!" ]
      , R.div' $ render d p s c
      ]

    stories = T.focus _topStoryList _StoryAction $
      T.foreach \_ -> storyItemSpec

    actions = T.simpleSpec performAction T.defaultRender

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
