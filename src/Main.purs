module Main where

import Prelude
import Web.HTML

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log, logShow)
import Web.DOM (Element)
import Web.DOM.Element (toNode)
import Web.DOM.Node (setTextContent, textContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType(..), target)
import Web.Event.Event (EventType(EventType))
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget, toParentNode)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.HTML.Window (document)


data Shape
  = Circle Number
  | Square Number
  | Rectangle Number Number
  | Triangle Number Number

derive instance genericShape :: Generic Shape _

instance shapeShow :: Show Shape where
  show x = genericShow x

area :: Shape -> Number
area (Circle radius) = 3.14 * radius * radius
area (Square side) = side * side
area (Rectangle width height) = width * height
area (Triangle side1 side2) = (side1 * side2) / 2.0

rootSelector :: QuerySelector
rootSelector = QuerySelector ("#root")

retriveText :: Maybe Element -> Effect String
retriveText maybeE = case maybeE of 
  Just e -> textContent (toNode e)
  Nothing -> pure ""

setMsg :: Maybe Element -> String -> Effect Unit
setMsg (Just e) msg = setTextContent msg (toNode e)
setMsg _ _ = pure unit

inputEvent :: EventType
inputEvent = EventType ("input")

-- inputEventHandler :: Maybe Element -> Event -> Effect Unit
-- inputEventHandler el evt = do
--   str <- eventValue evt
--   setMsg el str

inputValue :: Maybe HTMLInputElement -> Effect String
inputValue (Just el) = value el
inputValue _ = pure ""

targetValue :: Maybe EventTarget -> Effect String
targetValue (Just et) = inputValue (fromEventTarget et)
targetValue _ = pure ""

eventValue :: Event -> Effect String
eventValue evt = targetValue (target evt)

main :: Effect Unit
main = do
  w <- window
  doc <- document w
  ex <- querySelector rootSelector (toParentNode doc) 
  msg <- retriveText ex
  listener <- eventListener (\e -> (eventValue e) >>= (setMsg ex))
  addEventListener inputEvent listener true (toEventTarget doc)
  setMsg ex "Changing ~~~ Kris :) "
  log ("Yeah! " <> msg)
  log "🍝"
  logShow (Circle 3.0)
  logShow (Rectangle 3.0 4.0)
