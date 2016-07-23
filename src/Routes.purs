module App.Routes where

import Data.Functor ((<$), (<$>))
import Control.Apply ((<*), (*>))
import Data.Maybe (fromMaybe)
import Prelude (($))
import Control.Alt ((<|>))
import Pux.Router (end, router, lit, int)

data Route = Home | Track Int | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Track <$> (lit "tracks" *> int) <* end
