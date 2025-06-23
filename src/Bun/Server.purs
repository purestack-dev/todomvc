module Bun.Server (serve) where

import Prelude

import Bun.Request (Request)
import Bun.Response (Response)
import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)

foreign import serve_ :: (Request -> Effect (Promise Response)) -> Effect Unit

serve :: (Request -> Aff Response) -> Effect Unit
serve f = serve_ (\req -> Promise.fromAff (f req))
