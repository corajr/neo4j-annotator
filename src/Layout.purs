module App.Layout where

import App.Effects (AppEffects)
import App.Track as Track
import App.NotFound as NotFound
import App.Routes as Routes
import Prelude (($), map)
import Pux (EffModel, noEffects, mapState, mapEffects)
import Pux.Html (Html, div, h1, p, text)

data Action
  = TrackAct (Track.Action)
  | PageView Routes.Route

type State =
  { route :: Routes.Route
  , track :: Track.State }

init :: State
init =
  { route: Routes.NotFound
  , track: Track.init }

update :: Action -> State -> EffModel State Action AppEffects
update (PageView route@(Routes.Home)) state =
  mapState (_ { route = route }) (applyTrackAct (Track.Connect Track.serverInfo) state)
update (PageView route@(Routes.Track i)) state =
  mapState (_ { route = route }) (applyTrackAct (Track.Fetch i) state)
update (PageView route) state = noEffects $ state { route = route }
update (TrackAct action) state = applyTrackAct action state

applyTrackAct :: Track.Action -> State -> EffModel State Action AppEffects
applyTrackAct act state =
  mapEffects TrackAct (mapState (state { track = _ }) $ Track.update act state.track)

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , p [] [ text "Change src/Layout.purs and watch the page hot-reload." ]
    , case state.route of
        Routes.Home -> map TrackAct $ Track.view state.track
        Routes.Track _ -> map TrackAct $ Track.view state.track
        Routes.NotFound -> NotFound.view state
    ]
