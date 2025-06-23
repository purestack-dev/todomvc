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
  , Middleware
  ) where

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

data Middleware tag
