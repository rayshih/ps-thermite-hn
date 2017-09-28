module Data where

import Prelude

import Data.Lens (Lens', lens)
import Data.List (List, catMaybes)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Router (Route)
import Story (Story)

type State = { currentRoute :: Route
             , topStories   :: StrMap Story
             , topStoryIds  :: List Int
             }

_currentRoute :: Lens' State Route
_currentRoute = lens _.currentRoute (_ { currentRoute = _})

_topStoryList :: Lens' State (List Story)
_topStoryList = lens toList merge
  where
    toList :: State -> List Story
    toList st = catMaybes $ map (\id -> M.lookup (show id) st.topStories) st.topStoryIds

    merge :: State -> List Story -> State
    merge = const -- TODO this may useful for normalize

