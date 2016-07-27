module App.Track where

import Database.Neo4J
import Pux.Html.Attributes as Attr
import Pux.Html.Elements as El
import App.Effects (AppEffects)
import App.Secrets (neo4Jpassword)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)
import Data.Generic (class Generic, gShow)
import Data.List (List(..), (:))
import Data.Maybe (maybe)
import Data.Profunctor.Choice (left)
import Prelude (bind, show, ($), pure, (<$>), (*>), const, (>), (-), (<>), (<<<), unit, Unit, map, class Show)
import Pux (noEffects, EffModel)
import Pux.Html (Html, Attribute, div, span, dl, dt, dd, ul, li, button, text)
import Pux.Html.Attributes (title, href)
import Pux.Html.Events (onClick)
import Pux.Router (link, navigateTo)
import Signal.Channel (CHANNEL)

newtype Track = Track
  { id :: NeoInteger
  , title :: String
  , description :: String
  , tag_list :: String
  , uri :: String
  , permalink_url :: String
  }

derive instance genericTrack :: Generic Track
instance showTrack :: Show Track where
  show = gShow
instance isForeignTrack :: IsForeign Track where
  read = readGeneric defaultForeignOptions

newtype TrackSummary = TrackSummary
  { trackID :: NeoInteger, title :: String }

derive instance genericTrackSummary :: Generic TrackSummary
instance showTrackSummary :: Show TrackSummary where
  show = gShow
instance isForeignTrackSummary :: IsForeign TrackSummary where
  read = readGeneric defaultForeignOptions

data Action = Connect ConnectionInfo
            | OpenSession
            | GetTrackList
            | Fetch Int
            | SetProps Int Properties
            | MoveToNextTrack
            | Connected (Either String Driver)
            | SessionOpened (Either String Session)
            | ReceiveTrack (Either String Track)
            | ReceiveTrackList (Either String (Array TrackSummary))
            | SetStatus String
            | RunQueue
            | NoOp

data Properties = IsBallroom { props :: { isBallroomTrack :: Boolean} }
                | ContainsHa { props :: { containsHa :: Boolean } }

type State =
  { driver :: Either String Driver
  , session :: Either String Session
  , trackList :: Either String (Array TrackSummary)
  , track :: Either String Track
  , retries :: Int -- once retries are exhausted, don't try to connect anymore
  , status :: String
  , queuedActions :: List Action
  }

init :: State
init =
  { driver: Left "Not connected to server"
  , session: Left "No session started"
  , track: Left "Track not loaded"
  , trackList: Left "Tracklist not loaded"
  , retries: 3
  , status: ""
  , queuedActions: Nil
  }

serverInfo :: ConnectionInfo
serverInfo = ConnectionInfo
  { url: "bolt://localhost"
  , auth: mkAuth "neo4j" neo4Jpassword
  , connectionOpts: defaultConnectionOptions
  }

withDB :: Action -> State -> (Session -> Array (Aff (channel :: CHANNEL, err :: EXCEPTION | AppEffects) Action)) -> EffModel State Action AppEffects
withDB originalAction state f = case state.session of
  Left _ -> if state.retries > 0
            then { state: state { retries = state.retries - 1, queuedActions = originalAction: state.queuedActions}
                 , effects: [ pure (Connect serverInfo) ] }
            else { state: state, effects: [] }
  Right session -> { state: state, effects: f session }

infoFromSummary :: TrackSummary -> { url :: String, title :: String}
infoFromSummary (TrackSummary {trackID: (NeoInteger trackID'), title: title}) =
  { url: "/tracks/" <> show trackID'.low
  , title: title
  }

update :: Action -> State -> EffModel State Action AppEffects
update (Connect info) state =
  { state: state
  , effects: [ do
       driver <- attempt $ liftEff (mkDriver info)
       pure $ Connected (left show driver)
    ]
  }
update OpenSession state =
  case state.driver of
    Left _ -> noEffects state
    Right driver ->
      { state: state
      , effects: [ do session <- attempt $ liftEff (mkSession driver)
                      pure $ SessionOpened (left show session)
                 ]
      }
update (SessionOpened session) state =
  { state: state {session = session, queuedActions = GetTrackList:state.queuedActions }
  , effects: [ pure RunQueue ]
  }
update (Connected driver) state =
  { state: state { driver = driver }
  , effects: [ pure OpenSession ]
  }
update RunQueue state =
  case state.queuedActions of
    x:xs -> { state: state { queuedActions = xs }, effects: [ pure x, pure RunQueue ]}
    Nil -> noEffects state
update GetTrackList state =
  withDB GetTrackList state $ \session ->
    [ do tracks <- attempt $
           withTransaction session $ do
             query' (Query "MATCH (a:Track) WHERE NOT exists(a.isBallroomTrack) RETURN a.id as trackID, a.title as title" :: Query TrackSummary)
         pure (ReceiveTrackList $ left show tracks)
    ]
update MoveToNextTrack state =
  { state: state
  , effects: case state.trackList of
      Left _ -> []
      Right tracks -> maybe [] ((\({ url }) -> [ liftEff (navigateTo url) *> pure NoOp]) <<< infoFromSummary) (head tracks)
    }
update (ReceiveTrackList tracks) state =
  { state: state { trackList = tracks }
  , effects: [ pure MoveToNextTrack ]
  }
update org@(SetProps trackId props) state =
  withDB org state $ \session ->
    [ do case props of
           ContainsHa { props } -> withTransaction session $ setProps trackId props
           IsBallroom { props } -> withTransaction session $ setProps trackId props
         pure GetTrackList
    ]
update org@(Fetch trackId) state =
  withDB org state $ \session ->
    [ do track <- attempt $
           withTransaction session $ do
             query (Query "MATCH (x:Track) WHERE x.id = {id} RETURN x" :: Query' (Node Track)) (mkParams {id: trackId})
         pure (ReceiveTrack $
                 either (Left <<< show)
                        (\tracks -> maybe (Left "No tracks returned") (Right <<< getProperties <<< unbox) (head tracks))
                        track)
    ]
update (ReceiveTrack track) state =
  noEffects $ state { track = track }
update (SetStatus s) state = noEffects $ state { status = s }
update NoOp state = noEffects $ state


setProps :: forall eff a. Int -> a -> InTransaction eff Unit
setProps trackId props =
  execute (Query "MATCH (n:Track {id: {trackId}}) SET n += {props}") (mkParams {trackId: toNeoInt trackId, props: props })

viewTrackDetails :: Track -> Html Action
viewTrackDetails (Track { id, title, description, tag_list, permalink_url }) =
  dl [] [ dt [] [ text "ID" ]
        , dd [] [ text (show (unsafeFromNeoInt id)) ]
        , dt [] [ text "Title" ]
        , dd [] [ text title ]
        , dt [] [ text "Description" ]
        , dd [] [ text description ]
        , dt [] [ text "Tags" ]
        , dd [] [ text tag_list ]
        , dt [] [ text "URL" ]
        , dd [] [ El.a [ href permalink_url ] [ text permalink_url ] ]
        ]

foreign import encodeURIComponent :: String -> String

viewTrackEmbed :: String -> Html Action
viewTrackEmbed uri =
  El.iframe [ Attr.src ("https://w.soundcloud.com/player/?url=" <> encodeURIComponent uri) ] []

-- viewTrackAudio :: Int -> Html Action
-- viewTrackAudio trackID =
--   El.audio [ Attr.controls true, Attr.id_ ("track" <> show trackID) ]
--     [ El.source [ Attr.src ("http://127.0.0.1:8080/" <> show trackID <> ".mp3") ] []
--     , El.source [ Attr.src ("http://127.0.0.1:8080/" <> show trackID <> ".wav") ] []
--     ]

viewTrackAudio :: Int -> Html Action
viewTrackAudio trackID = fromReact [ Attr.attr "source" ("http://127.0.0.1:8080/" <> show trackID)] []

viewTrack :: Either String Track -> Html Action
viewTrack eitherTrack =
  div [] $
    case eitherTrack of
      Left err -> [ span [] [ text (show err) ] ]
      Right track@(Track rec) ->
        let trackID = unsafeFromNeoInt rec.id
        in [ viewTrackDetails track
           -- , viewTrackEmbed rec.uri
           , viewTrackAudio trackID
           , div [] [ button [ onClick (const (SetProps trackID (ContainsHa {props: { containsHa: true}})))] [ text "Contains Ha"] ]
           , div [] [ button [ onClick (const (SetProps trackID (IsBallroom {props: { isBallroomTrack: false}}))) ] [ text "Not ballroom"]
                    , button [ onClick (const (SetProps trackID (IsBallroom {props: { isBallroomTrack: true}}))) ] [ text "Ballroom"]
                    ]
           ]

viewTrackList :: Either String (Array TrackSummary) -> Html Action
viewTrackList tracks =
  div [] $
    case tracks of
      Left err -> [ span [] [ text (show err) ] ]
      Right tracks ->
        [ ul [] $
            map ((\({ url, title}) -> li [] [ link url [] [ text title ] ]) <<< infoFromSummary) tracks
        ]

view :: State -> Html Action
view state =
  div
    []
    [ div [] [ text state.status ]
    , viewTrack state.track
    , viewTrackList state.trackList
    ]

foreign import fromReact :: forall a.
                            Array (Attribute a) ->
                            Array (Html a) ->
                            Html a
