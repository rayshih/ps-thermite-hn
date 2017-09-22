module Router where

import Control.Alt ((<|>))
import Control.Applicative ((*>), (<$), (<*))
import Prelude (class Show, Unit)
import Routing.Match (Match)
import Routing.Match.Class (end, lit)

data Route
  = Top
  | Newest

instance showRoute :: Show Route where
  show Top = "Route Top"
  show Newest = "Route Newest"

root :: Match Unit
root = lit ""

routing :: Match Route
routing = Top <$ (root <* end)
      <|> Newest <$ (root *> lit "newest" <* end)
