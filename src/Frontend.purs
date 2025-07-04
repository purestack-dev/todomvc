module Frontend where

import Backend
import Plum.View
import Prelude
import Prim

import Data.Array ((:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.URL as URL
import Effect (Effect)
import Plum as Plum
import PureStack.Client (client)
import Unsafe.Coerce (unsafeCoerce)

type Model = { newTodo :: String, todos :: Array String }

data Msg
  = UpdateNewTodo String
  | AddNewTodo
  | TodoAdded

main :: Effect Unit
main = do
  Plum.run @Model @Msg "plum"
    { init: \_runMsg -> pure { newTodo: "", todos: [] }
    , update: \runMsg msg model -> case msg of
        UpdateNewTodo newTodo -> pure model { newTodo = newTodo }
        AddNewTodo -> do
          runMsg $ api.createTodo (TodoItem { text: model.newTodo, done: false }) $> TodoAdded
          pure model { newTodo = "", todos = model.newTodo : model.todos }
        TodoAdded -> pure model
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

          for_ model.todos $ \todo -> do
            text todo mempty
    }

url = case URL.fromString "http://localhost" of
  Just x -> x
  Nothing -> unsafeCoerce unit

api = client @API url

