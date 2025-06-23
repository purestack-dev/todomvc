module Bun.Request
  ( Request
  , method
  , url
  , json
  , headers
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut (Json)
import Effect.Aff (Aff)
import Foreign.Object (Object)

foreign import data Request :: Type

foreign import jsonPromise :: Request -> Promise Json

json :: Request -> Aff Json
json = jsonPromise >>> Promise.toAff

foreign import method :: Request -> String

foreign import url :: Request -> String

foreign import headers :: Request -> Object String
