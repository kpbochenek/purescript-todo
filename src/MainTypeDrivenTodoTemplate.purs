module MainTypeDrivenTodoTemplate where

import Data.Array
import Prelude

import Control.Apply (lift2)
import Data.Foldable (for_)
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Specular.Dom.Browser (Node)
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder (Builder)
import Specular.Dom.Element (Prop, attr, attrD, attrWhenD, attrs, bindChecked, bindValueOnChange, checkedD, class_, classes, dynText, el, el_, on, onClick_, text, valueD)
import Specular.Dom.Widget (class MonadWidget, Widget, emptyWidget, liftWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Input (getCheckboxChecked)
import Specular.FRP (Event, dynamic, holdDyn, newEvent, readDynamic, subscribeDyn_, subscribeEvent_, weaken, withDynamic_)
import Specular.FRP as Behaviour
import Specular.FRP as Dynamic
import Specular.FRP.Base (Dynamic, changed, foldDyn, subscribeEvent_)
import Specular.FRP.List (dynamicList_)
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

type Todo =
  { id :: Int
  , text :: String
  , completed :: Boolean
  }


data TodoState = Normal Todo | Updating Todo String 

type Todos = Array TodoState

type KeyboardEvent = { code :: String }


-- WIDGETS

keyUpCb :: Effect Unit -> DOM.Event -> Effect Unit
keyUpCb actionOnEnter e = if (unsafeCoerce e :: KeyboardEvent).code == "Enter" then actionOnEnter else pure unit

newTodoHtml :: Ref String -> Effect Unit -> Widget Unit
newTodoHtml inputTextTodo onEnterCb = 
    el "div" [classes ["input-group", "flex-nowrap"]] do
        el "span" [class_ "input-group-text", attr "id" "addon-wrapping", attr "placeholder" "todo"] $ text "New entry"
        el "input" [class_ "form-control", attr "aria-describedby" "addon-wrapping", attr "type" "text", bindValueOnChange inputTextTodo, 
        on "keyup" (keyUpCb onEnterCb)] emptyWidget


onId :: (Int -> Effect Unit) -> Dynamic Todo -> Effect Unit
onId action todoDyn = do
    todo <- Dynamic.readDynamic todoDyn
    action todo.id


unsafeEventTarget :: DOM.Event -> Node
unsafeEventTarget e = (unsafeCoerce e).target


onChecked :: (Boolean -> Effect Unit) -> Prop
onChecked cb = on "change" \event -> (getCheckboxChecked (unsafeEventTarget event)) >>= cb


toTodo :: TodoState -> Todo
toTodo (Normal todo) = todo
toTodo (Updating todo _) = todo


listElementHtml :: Dynamic TodoState -> (Int -> Effect Unit) -> (Int -> Effect Unit) -> (Int -> Boolean -> Effect Unit) -> Widget Unit
listElementHtml stateDyn deleteIdCb updateIdCb completeIdCb = do
    let todo = toTodo <$> stateDyn
    el "div" [classes ["d-flex", "border-bottom"]] do
      el "input" [classes ["p-2", "my-auto", "form-check-inline"], attr "type" "checkbox", attr "id" "checkboxNoLabel", checkedD $ (_.completed) <$> todo, onChecked (\b -> onId (\i -> completeIdCb i b) todo)] emptyWidget
      el "div" [classes ["p-2", "flex-grow-1", "my-auto"]] do
        el "span" [attrWhenD ((_.completed) <$> todo) "style" "text-decoration:line-through;"] do
          fieldBasedOnState stateDyn
      el "div" [classes ["p-2", "btn-group"]] do
        el "button" [classes ["btn", "btn-outline-secondary"], attr "type" "button", onClick_ (onId updateIdCb todo)] $ text "Edit"
        el "button" [classes ["btn", "btn-outline-secondary"], attr "type" "button", onClick_ (onId deleteIdCb todo)] $ text "Delete"

fieldBasedOnState :: Dynamic TodoState -> Widget Unit
fieldBasedOnState dynTodoS =
  withDynamic_ dynTodoS $ case _ of 
    Normal todo -> text $ "(" <> (show todo.id) <> ") " <> todo.text
    Updating todo txt -> do 
      text ("(" <> (show todo.id) <> ") ") 
      el "input" [attr "type" "text" ] emptyWidget

pureTodo :: Int -> String -> TodoState
pureTodo idx body = Normal { id: idx, text: body, completed: false }


newTodoFn :: Ref Int -> Ref String -> Ref Todos -> Effect Unit
newTodoFn counter inputText todos = do
    _ <- Ref.modify counter (_ + 1)
    newTodo <- lift2 pureTodo (Ref.read counter) (Ref.read inputText)
    Ref.modify todos (cons newTodo)
    Ref.write inputText ""
    
match :: Int -> TodoState -> Boolean
match idx (Normal todo) = todo.id == idx
match idx (Updating todo _) = todo.id == idx

deleteTodoFn :: Ref Todos -> Int -> Effect Unit
deleteTodoFn todos = 
    \idx -> Ref.modify todos (\items -> filter (not (match idx)) items)


completeTodo :: Int -> Boolean -> Todo -> Todo 
completeTodo idx c todo = if todo.id == idx then todo {completed = c} else todo


completeTodoS :: Int -> Boolean -> TodoState -> TodoState
completeTodoS idx c st@(Normal todo) = if todo.id == idx then Normal todo {completed = c} else st
completeTodoS idx c st@(Updating todo edit) = if todo.id == idx then Updating (todo { completed = c}) edit else st


editTodoS :: Int -> TodoState -> TodoState
editTodoS idx st@(Normal todo) = if todo.id == idx then Updating todo todo.text else st
editTodoS idx st@(Updating todo _) = if todo.id == idx then Normal todo else st

main :: Effect Unit
main = do
  runMainWidgetInBody do
    inputTextTodo :: Ref String <- Ref.new ""
    counter :: Ref Int <- Ref.new 3
    todos :: Ref Todos <- Ref.new [ Normal { id: 3, text: "ala", completed: false }, Normal { id: 2, text: "ma", completed: false }, Normal { id: 1, text: "kota", completed: false } ]

    let deleteTodoCb = deleteTodoFn todos
    let updateTodoCb = \i -> Ref.modify todos (map (editTodoS i))
    let completeTodoCb = \i b -> Ref.modify todos (map (completeTodoS i b))

    dynText $ ("Debug id: " <> _ ) <$> show <$> Ref.value counter
    el_ "hr" $ emptyWidget

    let onEnterCb = newTodoFn counter inputTextTodo todos

    el "div" [class_ "container"] do
        el "h2" [attr "text-center" "mt-3"] $ text "Todos:"
        newTodoHtml inputTextTodo onEnterCb
        el "div" [classes ["d-flex", "border-bottom"]] do
            el "div" [classes ["p-2", "flex-grow-1", "my-auto"]] do
                dynamicList_ (Ref.value todos) $ \item -> listElementHtml item deleteTodoCb updateTodoCb completeTodoCb
