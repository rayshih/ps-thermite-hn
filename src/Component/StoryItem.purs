module StoryItem where

import Prelude

import Action (StoryAction)
import React.DOM as R
import Story (Story(..))
import Thermite (Render)
import Thermite as T

storyItemSpec :: forall eff. T.Spec eff Story Unit StoryAction
storyItemSpec = T.simpleSpec T.defaultPerformAction render
  where
    render :: Render Story Unit StoryAction
    render dispatch _ (Story st) _ =
      [ R.div' [ R.text $ st.title ]
      ]


