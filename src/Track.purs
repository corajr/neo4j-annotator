module App.Track where

import Database.Neo4J
import App.Secrets (neo4Jpassword)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (either, Either(..))
import Data.Maybe (maybe)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (readGeneric)
import Data.Generic (class Generic, gEq, gShow)
import Data.Profunctor.Choice (left)
import Data.Array (head)
import Prelude (bind, show, ($), pure, class Show)
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, span, button, text)
import Pux.Html.Events (onClick)

newtype Track = Track
  { id :: Int
  , title :: String
  , description :: String
  , tag_list :: String
  , permalink_url :: String
  }

derive instance genericTrack :: Generic Track

instance showTrack :: Show Track where
  show = gShow

instance isForeignTrack :: IsForeign Track where
  read = readGeneric myForeignOpts

data Action = Connect ConnectionInfo
            | Connected (Either String Driver)
            | Fetch Int
            | ReceiveTrack (Either String Track)

type State =
  { driver :: Either String Driver
  , track :: Either String Track
  }

init :: State
init =
  { driver: Left "Not connected to server"
  , track: Left "Tracks not loaded"
  }

update :: Action -> State -> EffModel State Action (neo4j :: NEO4J)
update (Connect info) state =
  { state: state
  , effects: [ do
       driver <- attempt $ liftEff (mkDriver info)
       pure $ Connected (left show driver)
    ]
  }
update (Connected driver) state =
  noEffects $ state { driver = driver }
update (Fetch trackId) state =
  { state: state
  , effects: case state.driver of
      Left err -> [ ]
      Right driver -> [ do
        track <- attempt $ withSession driver $ \session ->
          withTransaction session $ do
            query (Query "MATCH (n:Track) WHERE n.id = {id} RETURN n" :: Query Track) (mkParams {id: trackId})
        pure (ReceiveTrack $ case track of
          Left err -> Left (show err)
          Right tracks -> maybe (Left "No tracks returned") Right (head tracks))
      ]
  }
update (ReceiveTrack track) state =
  noEffects $ state { track = track }

view :: State -> Html Action
view state =
  div
    []
    [ span [] [ text (show state.track) ]
    ]
