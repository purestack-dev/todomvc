module Backend where

import Prelude
import PureStack.Route

import Bun.Server as Bun
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import PureStack.Server as Server

type Foo = { a :: String, b :: Maybe Int, c :: Array Number }

type API =
  ( foo :: "foo" / "bar" / Foo / GET Unit { a :: Int, b :: Number, c :: Foo }
  , bar :: "foo" / Middleware "auth" / "qux" / POST (Headers { foo :: String } Unit) { a :: Int, s :: String }
  , deep ::
      "deep" /
        ( something :: "something" / GET Unit (Headers { foo :: String } { a :: String })
        )
  )

main :: Effect Unit
main = do
  log "🍝"
  Bun.serve $ Server.run @API identity
    { handlers:
        { foo: \x -> pure { a: 1, b: 1.0, c: x }
        , bar: \i (Headers x _) -> pure { a: 8, s: x.foo }
        , deep: { something: pure $ Headers { foo: "foo" } { a: "something" } }
        }
    , middlewares: { auth: \req -> pure 1 }
    }
