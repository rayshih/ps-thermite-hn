module Control where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Either (either)

parseOrThrow :: forall m a. MonadThrow Error m => DecodeJson a => Json -> m a
parseOrThrow json = either (throwError <<< error) pure $ decodeJson json
