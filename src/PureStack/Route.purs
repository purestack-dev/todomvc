module PureStack.Route
  ( type (/)
  , Slash
  , HttpMethod
  , class MethodName
  , methodName
  , GET
  , POST
  , PUT
  , DELETE
  , PATCH
  , Headers(..)
  , class ToHeaders
  , toHeaders
  , class ToHeader
  , toHeader
  , class FromHeaders
  , fromHeaders
  , class FromHeader
  , fromHeader
  , Middleware
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RowList
import PureStack.Cookie (Cookie)
import PureStack.Cookie as Cookie
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (Proxy(..))

data HttpMethod

-- | The `req` can only ever be `Unit`
foreign import data GET :: Type -> Type -> HttpMethod
foreign import data POST :: Type -> Type -> HttpMethod
foreign import data PUT :: Type -> Type -> HttpMethod
foreign import data DELETE :: Type -> Type -> HttpMethod
foreign import data PATCH :: Type -> Type -> HttpMethod

class MethodName :: HttpMethod -> Constraint
class MethodName method where
  methodName :: String

instance MethodName (GET req resp) where
  methodName = "GET"

instance MethodName (POST req resp) where
  methodName = "POST"

instance MethodName (PUT req resp) where
  methodName = "PUT"

instance MethodName (DELETE req resp) where
  methodName = "DELETE"

instance MethodName (PATCH req resp) where
  methodName = "PATCH"

data Slash :: forall k1 k2. k1 -> k2 -> Type
data Slash x y

-- | Operator for separating path pieces when specisying APIs.
-- | `Symbol`s are treated as constant path pieces.
-- | `Record`s are treated as query parameters. `Array`s are lists, `Maybe`s are optional query parameters. 
-- | Other types are captured path pieces.
infixr 1 type Slash as /

data Headers headers r = Headers headers r

class FromHeader t where
  fromHeader :: String -> Maybe t

instance FromHeader Cookie where
  fromHeader = Cookie.parse >>> case _ of
    Left _ -> Nothing
    Right x -> Just x

instance FromHeader String where
  fromHeader = Just

instance FromHeader Int where
  fromHeader = Int.fromString

class FromHeaders :: Row Type -> RowList Type -> Constraint
class FromHeaders row list | list -> row where
  fromHeaders :: Object String -> Maybe (Builder {} (Record row))

instance FromHeaders () RowList.Nil where
  fromHeaders _ = Just $ identity

instance
  ( FromHeader t
  , IsSymbol field
  , FromHeaders tail rest
  , Row.Cons field (Maybe t) tail row
  , Row.Lacks field tail
  ) =>
  FromHeaders row (RowList.Cons field (Maybe t) rest) where
  fromHeaders headers = do
    r <- fromHeaders @tail @rest headers
    case Object.lookup (reflectSymbol @field Proxy) headers of
      Nothing -> pure $ Builder.insert (Proxy @field) Nothing <<< r
      Just t -> do
        v <- fromHeader t
        pure $ Builder.insert (Proxy @field) (Just v) <<< r

else instance
  ( FromHeader t
  , IsSymbol field
  , FromHeaders tail rest
  , Row.Cons field t tail row
  , Row.Lacks field tail
  ) =>
  FromHeaders row (RowList.Cons field t rest) where
  fromHeaders headers = do
    t <- Object.lookup (reflectSymbol @field Proxy) headers >>= fromHeader
    r <- fromHeaders @tail @rest headers
    pure $ Builder.insert (Proxy @field) t <<< r

class ToHeader t where
  toHeader :: t -> String

instance ToHeader Cookie where
  toHeader = Cookie.stringify

instance ToHeader String where
  toHeader s = s

instance ToHeader Int where
  toHeader = show

class ToHeaders :: Row Type -> RowList Type -> Constraint
class ToHeaders row list where
  toHeaders :: Record row -> Object String

instance ToHeaders row RowList.Nil where
  toHeaders _ = Object.empty

instance
  ( ToHeaders row rest
  , Row.Cons field (Maybe t) tail row
  , IsSymbol field
  , ToHeader t
  ) =>
  ToHeaders row (RowList.Cons field (Maybe t) rest) where
  toHeaders headers =
    let
      rest = toHeaders @row @rest headers
    in
      case Record.get (Proxy @field) headers of
        Nothing -> rest
        Just val -> Object.insert (reflectSymbol @field Proxy) (toHeader val) rest

else instance
  ( ToHeaders row rest
  , Row.Cons field t tail row
  , IsSymbol field
  , ToHeader t
  ) =>
  ToHeaders row (RowList.Cons field t rest) where
  toHeaders headers =
    Object.insert (reflectSymbol @field Proxy) (toHeader (Record.get (Proxy @field) headers))
      (toHeaders @row @rest headers)

data Middleware :: Symbol -> Type
data Middleware tag
