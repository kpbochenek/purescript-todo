module MainTypeDrivenTodo where

import Data.Array (catMaybes, cons, filter, find, foldl, foldr, null)
import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect, foreachE)
import Effect.Class (liftEffect)
import Effect.Console (log)
import LocalStore (putL, getLL, delL)
import Simple.JSON as JSON
import Specular.Dom.Browser (Node)
import Specular.Dom.Browser as DOM
import Specular.Dom.Element (Prop, attr, attrWhenD, bindValueOnChange, checkedD, class_, classes, dynText, el, el_, on, onClick_, text)
import Specular.Dom.Widget (Widget, emptyWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Input (getCheckboxChecked)
import Specular.FRP (newEvent, subscribeDyn_, unlessD, whenD)
import Specular.FRP as Dynamic
import Specular.FRP.Base (Dynamic, subscribeEvent_)
import Specular.FRP.List (dynamicList_)
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)

type Todo =
  { id :: Int
  , text :: String
  , completed :: Boolean
  , editing :: Boolean
  }

type Todos = Array Todo

type TodoIds = Array String

type KeyboardEvent = { code :: String }

data TodoEvent = CompleteStateChange Int Boolean | EditingStateChange Int | DeleteTodo Int | ConfirmEditing Int | CompleteAll | RemoveCompleted

-- JSONS

decodeTodo :: String -> Maybe Todo
decodeTodo rawJson = case JSON.readJSON rawJson of
  Right (todo :: Todo) -> Just todo
  _ -> Nothing

encodeTodo :: Todo -> String
encodeTodo todo = JSON.writeJSON todo

decodeIds :: String -> Maybe TodoIds
decodeIds raw = case JSON.readJSON raw of
  Right (ids :: TodoIds) -> Just ids
  _ -> Nothing

encodeIds :: TodoIds -> String
encodeIds todoIds = JSON.writeJSON todoIds

-- WIDGETS

keyUpCb :: Effect Unit -> DOM.Event -> Effect Unit
keyUpCb actionOnEnter e = if (unsafeCoerce e :: KeyboardEvent).code == "Enter" then actionOnEnter else pure unit

newTodoHtml :: Ref String -> Effect Unit -> Widget Unit
newTodoHtml inputTextTodo onEnterCb = 
    el "div" [classes ["input-group", "flex-nowrap"]] do
        el "span" [class_ "input-group-text", attr "id" "addon-wrapping", attr "placeholder" "todo"] $ text "New entry"
        el "input" [class_ "form-control", attr "aria-describedby" "addon-wrapping", attr "type" "text", attr "autofocus" "true", bindValueOnChange inputTextTodo, 
        on "keyup" (keyUpCb onEnterCb)] emptyWidget

completeAllHtml :: (TodoEvent -> Effect Unit) -> Widget Unit
completeAllHtml fireEvent =
    el "button" [classes ["btn", "btn-outline-primary"], onClick_ (fireEvent CompleteAll)] $ text "Complete All"

removeCompletedHtml :: (TodoEvent -> Effect Unit) -> Widget Unit
removeCompletedHtml fireEvent =
    el "button" [classes ["btn", "btn-outline-secondary"], onClick_ (fireEvent RemoveCompleted)] $ text "Remove Completed"

onId :: (Int -> Effect Unit) -> Dynamic Todo -> Effect Unit
onId action todoDyn = do
    todo <- Dynamic.readDynamic todoDyn
    action todo.id

unsafeEventTarget :: DOM.Event -> Node
unsafeEventTarget e = (unsafeCoerce e).target


onChecked :: (Boolean -> Effect Unit) -> Prop
onChecked cb = on "change" \event -> (getCheckboxChecked (unsafeEventTarget event)) >>= cb


listElementHtml :: Ref String -> Dynamic Todo -> (TodoEvent -> Effect Unit) -> Widget Unit
listElementHtml editingTextRef todoDyn fireEvent = do
    el "div" [classes ["d-flex", "border-bottom"]] do
      el "input" [classes ["p-2", "my-auto", "form-check-inline"], attr "type" "checkbox", attr "id" "checkboxNoLabel",
          checkedD $ (_.completed) <$> todoDyn, onChecked (\b -> onId (\id -> fireEvent (CompleteStateChange id b)) todoDyn)] emptyWidget
      el "div" [classes ["p-2", "flex-grow-1", "my-auto"]] do
        el "span" [attrWhenD ((_.completed) <$> todoDyn) "style" "text-decoration:line-through;"] do
          fieldBasedOnState editingTextRef todoDyn fireEvent
      el "div" [classes ["p-2", "btn-group"]] do
        el "button" [classes ["btn", "btn-outline-secondary"], attr "type" "button", onClick_ (onId (\id -> fireEvent (EditingStateChange id)) todoDyn)] $ text "Edit"
        el "button" [classes ["btn", "btn-outline-secondary"], attr "type" "button", onClick_ (onId (\id -> fireEvent (DeleteTodo id)) todoDyn)] $ text "Delete"


fieldBasedOnState :: Ref String -> Dynamic Todo -> (TodoEvent -> Effect Unit) -> Widget Unit
fieldBasedOnState editingTextRef dynTodo fireEvent = do 
  let editingDyn = map (_.editing) dynTodo
  whenD (editingDyn) $ el "input" [attr "type" "text", bindValueOnChange editingTextRef,
     on "keyup" (keyUpCb (onId (\id -> fireEvent (ConfirmEditing id)) dynTodo)) ] emptyWidget
  unlessD (editingDyn) $ dynText (map (\todo -> "(" <> (show todo.id) <> ") " <> todo.text) dynTodo)

pureTodo :: Int -> String -> Todo
pureTodo idx body = { id: idx, text: body, completed: false , editing: false }


newTodoFn :: Ref Int -> Ref String -> Ref Todos -> Effect Unit
newTodoFn counter inputText todos = do
    _ <- Ref.modify counter (_ + 1)
    newTodo <- lift2 pureTodo (Ref.read counter) (Ref.read inputText)
    Ref.modify todos (cons newTodo)
    Ref.write inputText ""
    
deleteTodoFn :: Ref Todos -> Int -> Effect Unit
deleteTodoFn todos = 
    \idx -> do
      delL (show idx)
      Ref.modify todos (\items -> filter (\td -> not (td.id == idx)) items)



editTodo :: Ref Todos -> Int -> (Todo -> Todo) -> Effect Unit
editTodo todosRef idx fn = 
  Ref.modify todosRef (\todos -> (map (\todo -> if todo.id == idx then fn todo else todo)) todos)


handleEvents :: Ref String -> Ref Todos -> TodoEvent -> Effect Unit
handleEvents editingRef todosRef ev = case ev of
  CompleteStateChange id result ->
    editTodo todosRef id (\todo -> todo { completed = result })
  EditingStateChange id -> do
    txt <- Ref.read todosRef
    case (find (\todo -> todo.id == id) txt) of
      Just todo -> Ref.write editingRef todo.text
      Nothing -> pure unit
    editTodo todosRef id (\todo -> todo { editing = not todo.editing })
    Ref.modify todosRef (map (\todo -> if todo.editing == true && todo.id /= id then todo {editing = false} else todo))
  DeleteTodo id ->
    deleteTodoFn todosRef id
  ConfirmEditing id -> do
    editing <- Ref.read editingRef
    _ <- log $ "OK SET " <> editing
    editTodo todosRef id (\todo -> todo { editing = false, text = editing })
  CompleteAll ->
    Ref.modify todosRef (map (\todo -> todo { completed = true }))
  RemoveCompleted -> 
    Ref.modify todosRef (filter (\todo -> not todo.completed))


store :: Todos -> Effect Unit
store todos = do
  foreachE todos (\t -> putL (show t.id) (encodeTodo t))
  putL "IDS" (encodeIds (map (\t -> (show t.id)) todos))


loadFromStorageEntries :: Ref Todos -> TodoIds -> Effect Unit 
loadFromStorageEntries ref todoIds = do
  arrayRawMaybe <- traverse getLL todoIds
  Ref.write ref $ catMaybes (map (_ >>= decodeTodo) arrayRawMaybe)


loadFromStorage :: Ref Todos -> Effect Unit
loadFromStorage todos = do
  idsMaybe <- getLL "IDS"
  case idsMaybe >>= decodeIds of
    Just todoIds -> loadFromStorageEntries todos todoIds
    Nothing -> log "No ids persisted"


updateHighestId :: Ref Todos -> Ref Int -> Effect Unit
updateHighestId todos counter = do
  v <- Ref.read todos 
  let maxId = foldr max 0 $ map (_.id) v
  Ref.write counter maxId


main :: Effect Unit
main = do
  runMainWidgetInBody do
    inputTextTodo :: Ref String <- Ref.new ""
    editingTextTodo :: Ref String <- Ref.new ""

    counter :: Ref Int <- Ref.new 0
    todos :: Ref Todos <- Ref.new []

    liftEffect $ loadFromStorage todos
    liftEffect $ updateHighestId todos counter

    dynText $ ("Debug id: " <> _ ) <$> show <$> Ref.value counter
    el_ "hr" $ emptyWidget

    evt <- newEvent
    let event = evt.event
    let fire = evt.fire

    subscribeEvent_ (handleEvents editingTextTodo todos) event

    let onEnterCb = newTodoFn counter inputTextTodo todos

    let todosD = Ref.value todos
    let activeCountD = map (foldl (\acc todo -> if todo.completed then acc else acc + 1) 0) todosD
    let emptyListD = map null todosD

    subscribeDyn_ store todosD 

    el "div" [class_ "container"] do
        el "h2" [attr "text-center" "mt-3"] do
          dynText $ map (\c -> "Todos (" <> (show c) <> " items left):") activeCountD
        newTodoHtml inputTextTodo onEnterCb
        el "div" [classes ["d-flex", "border-bottom"]] do
            el "div" [classes ["p-2", "flex-grow-1", "my-auto"]] do
                whenD emptyListD $ el "div" [] $ text "There are no TODOs"
                dynamicList_ todosD $ \item -> listElementHtml editingTextTodo item fire
        completeAllHtml fire
        removeCompletedHtml fire
