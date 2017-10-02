module App where

import Prelude

import Action (Action(..), _StoryAction, _voidAction)
import Control (parseOrThrow)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (class MonadTrans, ExceptT(..), lift, runExceptT)
import Control.Parallel (parallel, sequential)
import Data (State, _currentRoute, _topStoryList)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Lens (over)
import Data.List (List, take)
import Data.Newtype (unwrap)
import Data.StrMap as M
import Data.Traversable (for, for_, traverse_)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Ajax
import React.DOM as R
import Router (Route, routing)
import Routing (matchesAff)
import Story (Story)
import StoryItem (storyItemSpec)
import Thermite (Render)
import Thermite as T

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

  (Tuple old new) <- lift $ matchesAff routing
  void $ T.modifyState \s -> s { currentRoute = new }

  where
    liftAff :: forall eff1 m a
             . MonadTrans m
            => Aff eff1 a
            -> ExceptT Error (m (Aff eff1)) a
    liftAff = ExceptT <<< lift <<< attempt

performAction _ _ _ = pure unit

routesSpec :: forall eff. T.Spec eff Route Unit Void
routesSpec = T.simpleSpec T.defaultPerformAction render
  where
    render :: Render Route Unit Void
    render dispatch _ route _ = [ R.div' [ R.text $ show route ] ]

spec :: forall eff. T.Spec (ajax :: AJAX, console :: CONSOLE | eff) State Unit Action
spec = container $ fold
  [ routeDisplay
  , stories
  , actions
  ]
  where
    routeDisplay = T.focus _currentRoute _voidAction routesSpec

    container = over T._render \render d p s c ->
      [ R.p' [ R.text "Hello thermite!!" ]
      , R.div' $ render d p s c
      ]

    stories = T.focus _topStoryList _StoryAction $
      T.foreach \_ -> storyItemSpec

    actions = T.simpleSpec performAction T.defaultRender
