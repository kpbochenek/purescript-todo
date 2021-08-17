module MainTypeDrivenTodoFail where

import Data.Array
import Prelude

import Control.Apply (lift2)
import Data.Foldable (for_)
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.FS (FileFlags(..))
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
import Specular.Ref (Ref, Lens)
import Specular.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

type Todo =
  { id :: Int
  , text :: String
  , completed :: Boolean
  }


data TodoState = Normal Todo | Updating Todo String 

data TodoElement = TodoElement Int (Ref TodoState)

type Todos = Array TodoElement

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


-- TODO STATE
mod :: (Todo -> Todo) -> TodoState -> TodoState
mod fn (Normal todo) = Normal (fn todo)
mod fn (Updating todo up) = Updating (fn todo) up





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

swap :: TodoState -> TodoState
swap (Normal todo) = Updating todo todo.text
swap (Updating todo _) = Normal todo

completedLens :: Lens TodoState Boolean
completedLens =
 { get: (_.completed) <<< toTodo
 , set: \ts b -> mod (_ {completed = b}) ts
 }


listElementHtml :: Dynamic TodoElement -> (Int -> Effect Unit) -> Widget Unit
listElementHtml elementDyn deleteIdCb = do
    let todo = map (\(TodoElement _ todoRef) -> todoRef) elementDyn
    let todoD = todo >>= Ref.value
    todoRef <- Dynamic.readDynamic todo
    el "div" [classes ["d-flex", "border-bottom"]] do
      el "input" [classes ["p-2", "my-auto", "form-check-inline"], attr "type" "checkbox", attr "id" "checkboxNoLabel",
          bindChecked $ Ref.pureFocusRef completedLens todoRef] emptyWidget
      el "div" [classes ["p-2", "flex-grow-1", "my-auto"]] do
        el "span" [attrWhenD ((_.completed) <$> (map toTodo (Ref.value todoRef))) "style" "text-decoration:line-through;"] do
          fieldBasedOnState todoD
      el "div" [classes ["p-2", "btn-group"]] do
        el "button" [classes ["btn", "btn-outline-secondary"], attr "type" "button", onClick_ (Ref.modify todoRef swap)] $ text "Edit"
        el "button" [classes ["btn", "btn-outline-secondary"], attr "type" "button", onClick_ (onId deleteIdCb (map toTodo (Ref.value todoRef)))] $ text "Delete"

fieldBasedOnState :: Dynamic TodoState -> Widget Unit
fieldBasedOnState state =
  withDynamic_ state $ case _ of 
    Normal todo -> text $ "(" <> (show todo.id) <> ") " <> todo.text
    Updating todo txt -> do 
      text ("(" <> (show todo.id) <> ") ") 
      el "input" [attr "type" "text"] emptyWidget

pureTodo :: Int -> String -> TodoState
pureTodo idx body = Normal { id: idx, text: body, completed: false }


newTodoFn :: Ref Int -> Ref String -> Ref Todos -> Effect Unit
newTodoFn counter inputText todos = do
    _ <- Ref.modify counter (_ + 1)
    newTodo <- lift2 pureTodo (Ref.read counter) (Ref.read inputText)
    newTodoRef <- Ref.new newTodo
    Ref.modify todos (cons (TodoElement (toTodo newTodo).id newTodoRef))
    Ref.write inputText ""
    
match :: Int -> TodoElement -> Boolean
match idx (TodoElement id todoRef) = id == idx

deleteTodoFn :: Ref Todos -> Int -> Effect Unit
deleteTodoFn todos = 
    \idx -> Ref.modify todos (\items -> filter (not (match idx)) items)


main :: Effect Unit
main = do
  runMainWidgetInBody do
    inputTextTodo :: Ref String <- Ref.new ""
    counter :: Ref Int <- Ref.new 3
    t1 :: Ref TodoState <- Ref.new $ Normal { id: 1, text: "kot", completed: false }
    t2 :: Ref TodoState <- Ref.new $ Normal { id: 2, text: "ma", completed: false }
    t3 :: Ref TodoState <- Ref.new $ Normal { id: 3, text: "Ala", completed: true }

    todos :: Ref Todos <- Ref.new 
      [ TodoElement 3 t3
      , TodoElement 2 t2
      , TodoElement 1 t1
      ]

    let deleteTodoCb = deleteTodoFn todos

    dynText $ ("Debug id: " <> _ ) <$> show <$> Ref.value counter
    el_ "hr" $ emptyWidget

    let onEnterCb = newTodoFn counter inputTextTodo todos

    el "div" [class_ "container"] do
        el "h2" [attr "text-center" "mt-3"] $ text "Todos:"
        newTodoHtml inputTextTodo onEnterCb
        el "div" [classes ["d-flex", "border-bottom"]] do
            el "div" [classes ["p-2", "flex-grow-1", "my-auto"]] do
                dynamicList_ (Ref.value todos) $ \item -> listElementHtml item deleteTodoCb
