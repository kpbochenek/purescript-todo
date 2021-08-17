module SpecularCounterInit where

import Data.Array
import Prelude

import Data.List as L
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Specular.Dom.Builder (Builder)
import Specular.Dom.Element (attr, attrD, class_, classes, dynText, el, onClick_, text)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.FRP (newEvent, readDynamic, withDynamic_)
import Specular.FRP as Dynamic
import Specular.FRP.Base (Dynamic, changed, subscribeEvent_)
import Specular.FRP.List (dynamicList, dynamicListWithIndex)
import Specular.Ref (Ref, focusRef)
import Specular.Ref as Ref

type Todo = { id :: Int, text :: String, completed :: Boolean }


-- listLayoutHtml :: forall x. (Array Todo) -> Builder x Unit
-- listLayoutHtml todos = 
--   el "div" [class_ "list-group"] do
--     L.foldMap (listItemHtml) todos

--listItemHtml :: forall x. Todo -> rmCb -> Builder x Unit
listItemHtml todo deleteCb = 
    el "a" [classes ["list-group-item", "list-group-item-action"]] do
      el "p" [class_ "mb-1"] do
        _ <- if todo.completed then el "input" [attr "type" "checkbox", attr "id" "checkboxNoLabel", attr "checked" "x"] (text "")
             else el "input" [attr "type" "checkbox", attr "id" "checkboxNoLabel"] (text "")
        text $ (show todo.id) <> todo.text
        el "button" [attr "type" "button", classes ["badge", "btn-danger"], onClick_ (deleteCb todo.id)] do
          text "x"




pureTodo :: Int -> Todo
pureTodo idx = { id: idx, text: "", completed: false }

todoDiv = 
  el "div" [classes ["d-inline-flex", "p-2", "bd-highlight"]] do
    el "textarea" [classes ["form-control"]] do
      text "empty"

newTodoButton :: forall x. Effect Unit -> Builder x Unit
newTodoButton cb =
  el "button" [classes ["btn", "btn-primary"], attr "type" "button", onClick_ cb ] do
    text "NEW"

--rend :: forall x. Int -> Dynamic Todo -> Builder x Unit
renderItem deleteCb dyn = do
    e <- readDynamic dyn
    listItemHtml e deleteCb

-- newTodo ref = do
--     id <- Ref.read ref
--     Ref.modify ref (\x -> x + 1)
--     pureTodo id

newTodoFn :: Ref Int -> Ref (Array Todo) -> Effect Unit
newTodoFn counter todos = do
    _ <- Ref.modify counter (\i -> i + 1)
    newTodo <- map (pureTodo) (Ref.read counter)
    Ref.modify todos (cons newTodo)
    
deleteTodoFn :: Ref (Array Todo) -> Int -> Effect Unit
deleteTodoFn todos = 
    \idx -> Ref.modify todos (\items -> filter (\e -> e.id /= idx) items)

main :: Effect Unit
main = do
  -- | Will append widget to the body

  runMainWidgetInBody do
    counter :: Ref Int <- Ref.new 0
    todos :: Ref (Array Todo) <- Ref.new []

    -- let addTodoCb = (Ref.modify todos) (cons (pureTodo 1))

    -- let rmTodoCb = \id -> (Ref.modify todos) (filter (\t -> t.id /= id)) 

    let x = Ref.value todos

    let newTodoCb = newTodoFn counter todos
    let deleteTodoCb = deleteTodoFn todos

    _ <- dynamicList x (renderItem deleteTodoCb)

    -- subscribeEvent_ (\h -> unit) (changed x)


    -- withDynamic_ x listLayoutHtml

    newTodoButton newTodoCb

    dynText $ show <$> Ref.value counter
