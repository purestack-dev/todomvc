module Backend where

import Prelude
import PureStack.Route

import Bun.Server as Bun
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.URL as URL
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import ProjectM36 (class RelVar, define, insert, lit, query, rel, relVar, withTransaction)
import ProjectM36 as M36
import PureStack.Client (class ToRequest)
import PureStack.Server (class FromRequest)
import PureStack.Server as Server

newtype TodoItem = TodoItem { text :: String, done :: Boolean }

derive instance Newtype TodoItem _
derive newtype instance EncodeJson TodoItem
derive newtype instance DecodeJson TodoItem
derive newtype instance ToRequest TodoItem
derive newtype instance FromRequest TodoItem

instance RelVar TodoItem where
  relVar = "todo-item"

type API =
  ( getTodos :: "todos" / GET Unit (Array TodoItem)
  , createTodo :: "todos" / POST TodoItem Unit
  )

main :: Effect Unit
main = do
  m36Connection <- case URL.fromString "http://localhost:6543" of
    Just url -> pure $ M36.Connection url
    Nothing -> throw "Invalid m36 URL"

  launchAff_ do
    liftEffect $ log "Creating relational variables"
    withTransaction m36Connection do
      define @TodoItem
    liftEffect $ log "Done"

    liftEffect $ Bun.serve $ Server.run @API identity
      { handlers:
          { getTodos: do
              liftAff $ withTransaction m36Connection $ do
                (map <<< map) wrap <$> query $ rel @TodoItem
          , createTodo: \todo -> do
              liftAff $ withTransaction m36Connection $ do
                insert @TodoItem $ lit [ todo ]
              pure unit
          }
      , middlewares: {}
      }
