module Client where

import Prelude
import App.Events (AppEffects, Event(..), foldp)
import App.Routes (match)
import App.State (State, init)
import App.View.Layout (view)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Data.Foldable (sequence_)
import Data.List (singleton)
import Data.Maybe (Maybe(..))
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal (runSignal, (~>))
import Signal.Channel (send)
import WebSocket (Connection(..), Message(..), URL(..), WEBSOCKET, newWebSocket, runMessage, runMessageEvent)

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (ws :: WEBSOCKET, err :: EXCEPTION, history :: HISTORY, dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [routeSignal] }

  let handleWS ev = case ev of
        (OpenWS) -> do
          socket@(Connection s) <- newWebSocket (URL "ws://echo.websocket.org") []
          s.onopen $= \event -> do
            send app.input $ singleton $ OpenedWS socket
        (OpenedWS (Connection s)) -> do
          s.onmessage $= \event -> do
            let msg = runMessage (runMessageEvent event)
            send app.input $ singleton $ ReceiveWS msg
        (SendWS msg (Just (Connection s))) -> s.send $ Message msg
        _ -> pure unit

  runSignal $ app.events ~> sequence_ <<< map handleWS

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app

initialState :: State
initialState = init "/"
