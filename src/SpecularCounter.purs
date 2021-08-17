module SpecularCounter where

import Data.Array
import Prelude

import Data.Foldable (for_)
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Specular.Dom.Builder (Builder)
import Specular.Dom.Element (attr, attrD, class_, classes, dynText, el, onClick_, text)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.FRP (weaken, dynamic, holdDyn, newEvent, readDynamic, subscribeDyn_, subscribeEvent_, withDynamic_)
import Specular.FRP as Behaviour
import Specular.FRP as Dynamic
import Specular.FRP.Base (Dynamic, changed, foldDyn, subscribeEvent_)
import Specular.FRP.List (dynamicList, dynamicList_, dynamicListWithIndex, weakDynamicList)
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
        el "button" [attr "type" "button", classes ["badge", "btn-danger"], onClick_ (deleteCb 2)] do
          text "x"


worka cb todo = do
  t <- Behaviour.readBehavior $ Dynamic.current todo
  cb t.id

listItemHtmlD todo deleteCb = 
    el "a" [classes ["list-group-item", "list-group-item-action"]] do
      el "p" [class_ "mb-1"] do
        el "input" [attr "type" "checkbox", attr "id" "checkboxNoLabel"] (text "")
        dynText $ map (\t -> (show t.id) <> t.text) todo
        el "button" [attr "type" "button", classes ["badge", "btn-danger"], onClick_ (worka deleteCb todo)] do
          text "x"



pureTodo :: Int -> Todo
pureTodo idx = { id: idx, text: (show idx) <> ": yay", completed: false }

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

newTodoFn :: Ref Int -> Ref (Array Todo) -> (Int -> Effect Unit) -> Effect Unit
newTodoFn counter todos fire = do
    _ <- Ref.modify counter (\i -> i + 1)
    newTodo <- map (pureTodo) (Ref.read counter)
    Ref.modify todos (cons newTodo)
    fire 1
    
deleteTodoFn :: Ref (Array Todo) -> (Int -> Effect Unit) -> Int -> Effect Unit
deleteTodoFn todos fire = 
    \idx -> do
      Ref.modify todos (\items -> filter (\e -> e.id /= idx) items)
      fire 1


refreshList :: forall rio. Ref (Array Todo) -> (Int -> Effect Unit) -> Builder rio Unit
refreshList todosR deleteCb = do
  todos <- Ref.read todosR
  foldMap (\td -> listItemHtml td deleteCb) todos
    

main :: Effect Unit
main = do
  -- | Will append widget to the body

  runMainWidgetInBody do
    counter :: Ref Int <- Ref.new 0
    todos :: Ref (Array Todo) <- Ref.new []

    -- let addTodoCb = (Ref.modify todos) (cons (pureTodo 1))

    -- let rmTodoCb = \id -> (Ref.modify todos) (filter (\t -> t.id /= id)) 
    modifyList <- newEvent
    let ev = modifyList.event
    let fire = modifyList.fire

    let newTodoCb = newTodoFn counter todos fire
    let deleteTodoCb = deleteTodoFn todos fire

    -- _ <- dynamicList x (renderItem deleteTodoCb)


    dynEv <- holdDyn 1 ev

    --dynamicList

    --foldDyn
    let dn = Ref.value todos
    let wdn = weaken dn

    dynamicList_ dn $ \item -> do
      listItemHtmlD item deleteTodoCb
      --el "p" [] $ dynText $ map (\t -> t.text) item

    --subscribeEvent_ (\h -> main) ev

    --withDynamic_ dynEv (\_ -> (refreshList todos deleteTodoCb))

    -- withDynamic_ x listLayoutHtml

    newTodoButton newTodoCb

    dynText $ show <$> Ref.value counter