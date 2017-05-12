module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import WebSocket (Connection)

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , socket :: Maybe Connection
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , socket: Nothing
  }
