module Action where

import Prelude

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Router (Route)

data StoryAction

data Action = RootDidMount
            | StoryAction Int StoryAction
            | RouteTo Route

_StoryAction :: Prism' Action (Tuple Int StoryAction)
_StoryAction = prism' (uncurry StoryAction) \a ->
  case a of
    (StoryAction idx sa)-> Just (Tuple idx sa)
    _ -> Nothing

_voidAction :: Prism' Action Void
_voidAction = prism' absurd (const Nothing)
