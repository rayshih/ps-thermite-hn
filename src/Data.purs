module Data where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Router (Route)

newtype Story = Story { title :: String
                      , id :: Int
                      }

instance decodeJsonStory :: DecodeJson Story where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    pure $ Story { id, title }

derive instance newtypeStory :: Newtype Story _

type State = { currentRoute :: Route
             , topStories   :: StrMap Story
             , topStoryIds  :: List Int
             }
