module Bun.Response where

import Data.Argonaut (Json)
import Foreign.Object (Object)

foreign import data Response :: Type
foreign import data ReadableStream :: Type

type Options = { status :: Int, statusText :: String, headers :: Object String }

foreign import json :: Json -> Options -> Response
foreign import string :: String -> Options -> Response
foreign import stream :: ReadableStream -> Options -> Response
foreign import body :: Response -> ReadableStream
foreign import headers :: Response -> Object String
foreign import status :: Response -> Int
foreign import statusText :: Response -> String

