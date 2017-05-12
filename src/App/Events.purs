module App.Events where

import Prelude
import App.Routes (Route)
import App.State (State(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import WebSocket (Connection)

data Event
  = PageView Route
  | OpenWS
  | OpenedWS Connection
  | ReceiveWS String
  | SendWS String (Maybe Connection)

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State s) = noEffects $ State s { route = route, loaded = true }
foldp (OpenedWS socket) (State s) = noEffects $ State s { socket = Just socket }
foldp _ st = noEffects $ st
