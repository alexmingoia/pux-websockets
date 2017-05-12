module App.View.Homepage where

import App.Events (Event(..))
import App.State (State(..))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, button, div, h1)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), (#!), text)
import Prelude hiding (div)

view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Pux"
    button #! onClick (const OpenWS) $ text "Open WS"
    button #! onClick (const (SendWS "echo" s.socket)) $ text "Echo"
    a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
