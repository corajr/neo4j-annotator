module App.Layout where

import App.Track as Track
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux (EffModel, noEffects, mapState, mapEffects)
import Pux.Html (Html, div, h1, p, text)

import Database.Neo4J (NEO4J)

data Action
  = TrackAct (Track.Action)
  | PageView Route

type State =
  { route :: Route
  , track :: Track.State }

init :: State
init =
  { route: NotFound
  , track: Track.init }

update :: Action -> State -> EffModel State Action (neo4j :: NEO4J)
update (PageView route) state = noEffects $ state { route = route }
update (TrackAct action) state =
  mapEffects TrackAct (mapState (state { track = _ }) $ Track.update action state.track)

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch the page hot-reload." ]
    , case state.route of
        Home -> map TrackAct $ Track.view state.track
        NotFound -> NotFound.view state
    ]
