module Frontend where

import Backend
import Plum.View
import Prelude
import Prim

import Data.Array ((:))
import Data.Foldable (for_)
import Data.URL as URL
import Effect (Effect)
import Plum as Plum
import PureStack.Client (client)

type Model = { newTodo :: String, todos :: Array TodoItem }

data Msg
  = UpdateNewTodo String
  | SetTodos (Array TodoItem)
  | AddNewTodo
  | TodoAdded

main :: Effect Unit
main = do
  origin <- Plum.currentOrigin
  let api = getApi (origin `URL.addSegment` "api")
  Plum.run @Model @Msg "plum"
    { init: \runMsg -> do
        runMsg $ SetTodos <$> api.getTodos
        pure { newTodo: "", todos: [] }
    , update: \runMsg msg model -> case msg of
        UpdateNewTodo newTodo -> pure model { newTodo = newTodo }
        AddNewTodo -> do
          runMsg $ api.createTodo (TodoItem { text: model.newTodo, done: false }) $> TodoAdded
          pure model
        TodoAdded -> do
          runMsg $ SetTodos <$> api.getTodos
          pure model
        SetTodos todos -> pure model { todos = todos }
    , view: \model -> column do
        font "sans-serif"

        column $ height (Px 30)
        text "todos" do
          fontSize 80
          fontColor (rgb255 184 63 69)
        column $ do
          width (Px 550)

          textInput { onChange: UpdateNewTodo, value: model.newTodo } do
            fontSize 24

            onKey "Enter" AddNewTodo

          for_ model.todos $ \(TodoItem todo) -> do
            text todo.text mempty
    }

getApi = client @API

