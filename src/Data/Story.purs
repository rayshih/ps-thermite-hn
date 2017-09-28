module Story where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Newtype (class Newtype)

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
