module PureStack.Server
  ( Server
  , Response(..)
  , ResponseBody(..)
  , class ToResponse
  , toResponse
  , class FromRequest
  , fromRequest
  , run
  , notFound
  , internalServerError
  , badRequest
  -------------------------------------------------------------------------------------------------
  , class ServeAPI
  , serveAPI
  , Nt(..)
  , class ServeRoute
  , serveRoute
  , class ServeQuery
  , serveQuery
  , class ParsePathPiece
  , parsePathPiece
  , class FromHeaders
  , fromHeaders
  , class FromHeader
  , fromHeader
  ) where

import Prelude
import PureStack.Route

import Bun.Request (Request)
import Bun.Request as Bun
import Bun.Request as Request
import Bun.Response as Bun.Response
import Control.Alternative (class Plus, empty)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import PureStack.Cookie (Cookie)
import PureStack.Cookie as Cookie
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

type Route =
  { path :: Array String
  , verb :: String
  , query :: Map String (Array String)
  , originalRequest :: Bun.Request
  }

routeFromRequest :: Request -> Route
routeFromRequest req =
  let
    url = URL.fromString $ Request.url req
  in
    { verb: Request.method req
    , path: case url of
        Nothing -> []
        Just u -> case URL.path u of
          URL.PathEmpty -> []
          URL.PathAbsolute p -> p
          URL.PathRelative p -> p
    , query: case url of
        Nothing -> Map.empty
        Just u -> URL.query u
    , originalRequest: req
    }

type Server a = ExceptT Response Aff a

newtype Nt m = Nt (forall x. m x -> Server x)

class ServeRoute :: forall k. k -> Type -> Row Type -> (Type -> Type) -> Constraint
class ServeRoute route handler middlewares m | route middlewares -> handler, handler -> m where
  serveRoute :: Route -> Maybe (Nt m -> handler -> Record middlewares -> Request -> Server Response)

instance (ServeRoute rest handler middlewares m, IsSymbol path) => ServeRoute (path / rest) handler middlewares m where
  serveRoute route@{ path } = do
    { head: h, tail } <- Array.uncons path
    assert $ h == (reflectSymbol @path Proxy)
    serveRoute @rest route { path = tail }

else instance
  ( ServeRoute rest handler middlewares m
  , ServeQuery row list
  , RowToList row list
  ) =>
  ServeRoute (Record row / rest) (Record row -> handler) middlewares m where
  serveRoute route@{ query } = do
    builder <- serveQuery @row @list query
    serveRoute @rest route <#> (\f nt handler req -> f nt (handler $ Builder.buildFromScratch builder) req)

else instance
  ( ServeRoute rest handler middlewares m
  , IsSymbol field
  , Row.Cons field (Bun.Request -> m x) tail middlewares
  ) =>
  ServeRoute (Middleware field / rest) (x -> handler) middlewares m where
  serveRoute route@{ originalRequest } = do
    serveRoute @rest route <#>
      ( \f nt@(Nt ntf) handler middlewares req -> do
          x <- ntf $ Record.get (Proxy @field) middlewares originalRequest
          f nt (handler x) middlewares req
      )

else instance (ServeRoute rest handler middlewares m, ParsePathPiece t) => ServeRoute (t / rest) (t -> handler) middlewares m where
  serveRoute route@{ path } = do
    { head: h, tail } <- Array.uncons path
    t <- parsePathPiece @t h
    serveRoute @rest route { path = tail } <#> (\f nt handler mws req -> f nt (handler t) mws req)

else instance MethodName (meth Unit Unit) => ServeRoute (meth Unit Unit) (m Unit) middlewares m where
  serveRoute { path, verb } = do
    assert $ verb == methodName @(meth Unit Unit)
    assert $ path == []
    pure $ \(Nt nt) handler _middlewares _req -> do
      nt handler
      pure $ ok
else instance (ToResponse resp, MethodName (meth Unit resp)) => ServeRoute (meth Unit resp) (m resp) middlewares m where
  serveRoute { path, verb } = do
    assert $ verb == methodName @(meth Unit resp)
    assert $ path == []
    pure $ \(Nt nt) handler _middlewares _req -> do
      resp <- nt handler
      pure $ toResponse resp
else instance (FromRequest req, MethodName (meth resp Unit)) => ServeRoute (meth resp Unit) (req -> m Unit) middlewares m where
  serveRoute { path, verb } = do
    assert $ verb == methodName @(meth resp Unit)
    assert $ path == []
    pure $ \(Nt nt) handler _middlewares req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          nt $ handler r
          pure $ ok
else instance (FromRequest req, ToResponse resp, MethodName (meth req resp)) => ServeRoute (meth req resp) (req -> m resp) middlewares m where
  serveRoute { path, verb } = do
    assert $ verb == methodName @(meth req resp)
    assert $ path == []
    pure $ \(Nt nt) handler _middlewares req -> do
      liftAff (fromRequest req) >>= case _ of
        Nothing -> pure $ badRequest
        Just r -> do
          resp <- nt $ handler r
          pure $ toResponse resp

else instance
  ( ServeAPI routes list handlers middlewares m
  , RowToList routes list
  ) =>
  ServeRoute routes (Record handlers) middlewares m where
  serveRoute route = pure $ \nt handlers middlewares req ->
    serveAPI @routes @list nt handlers middlewares route req

class ServeQuery :: Row Type -> RowList Type -> Constraint
class ServeQuery row list | list -> row where
  serveQuery :: Map String (Array String) -> Maybe (Builder (Record ()) (Record row))

instance ServeQuery () RowList.Nil where
  serveQuery _ = Just $ identity

instance
  ( ParsePathPiece t
  , ServeQuery tail rest
  , IsSymbol field
  , Row.Cons field (Array t) tail row
  , Row.Lacks field tail
  ) =>
  ServeQuery row (RowList.Cons field (Array t) rest) where
  serveQuery m = do
    r <- serveQuery @tail @rest m
    case Map.lookup (reflectSymbol @field Proxy) m of
      Nothing -> pure $ Builder.insert (Proxy @field) [] <<< r
      Just a -> do
        v <- parsePathPiece @t `traverse` a
        pure $ Builder.insert (Proxy @field) v <<< r

else instance
  ( ParsePathPiece t
  , ServeQuery tail rest
  , IsSymbol field
  , Row.Cons field (Maybe t) tail row
  , Row.Lacks field tail
  ) =>
  ServeQuery row (RowList.Cons field (Maybe t) rest) where
  serveQuery m = do
    r <- serveQuery @tail @rest m
    case Map.lookup (reflectSymbol @field Proxy) m of
      Nothing -> pure $ Builder.insert (Proxy @field) Nothing <<< r
      Just a -> do
        { head, tail } <- Array.uncons a
        assert $ tail == []
        v <- parsePathPiece @t head
        pure $ Builder.insert (Proxy @field) (Just v) <<< r

else instance
  ( ParsePathPiece t
  , ServeQuery tail rest
  , IsSymbol field
  , Row.Cons field t tail row
  , Row.Lacks field tail
  ) =>
  ServeQuery row (RowList.Cons field t rest) where
  serveQuery m = do
    { head, tail } <- Map.lookup (reflectSymbol @field Proxy) m >>= Array.uncons
    assert $ tail == []
    v <- parsePathPiece @t head
    r <- serveQuery @tail @rest m
    pure $ Builder.insert (Proxy @field) v <<< r

data ResponseBody
  = JsonResponseBody Json
  | EmptyResponseBody
  | StringResponseBody String

newtype Response = Response { body :: ResponseBody, status :: Int, statusText :: String, headers :: Object String }

class ToResponse r where
  toResponse :: r -> Response

instance EncodeJson (Record row) => ToResponse (Record row) where
  toResponse rec = Response { body: JsonResponseBody $ encodeJson rec, status: 200, statusText: "OK", headers: Object.empty }

instance ToResponse Response where
  toResponse resp = resp

class FromRequest r where
  fromRequest :: Request -> Aff (Maybe r)

instance DecodeJson (Record row) => FromRequest (Record row) where
  fromRequest req = Request.json req <#> \j -> case decodeJson j of
    Right r -> Just r
    Left _ -> Nothing

instance FromRequest Request where
  fromRequest req = pure $ Just req

instance FromRequest Unit where
  fromRequest _ = pure $ Just unit

instance
  ( FromRequest req
  , FromHeader header
  ) =>
  FromRequest (Headers (Object header) req) where
  fromRequest req = do
    r <- fromRequest req
    pure $ do
      headers <- traverse fromHeader (Request.headers req)
      r' <- r
      pure $ Headers headers r'

instance
  ( RowToList headers headersList
  , FromRequest req
  , FromHeaders headers headersList
  ) =>
  FromRequest (Headers (Record headers) req) where
  fromRequest req = do
    r <- fromRequest req
    pure $ do
      headers <- fromHeaders @headers @headersList (Request.headers req)
      r' <- r
      pure $ Headers (Builder.buildFromScratch headers) r'

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

class ParsePathPiece t where
  parsePathPiece :: String -> Maybe t

instance ParsePathPiece String where
  parsePathPiece = Just

instance ParsePathPiece Int where
  parsePathPiece = Int.fromString

instance ParsePathPiece Number where
  parsePathPiece = Number.fromString

instance ParsePathPiece Boolean where
  parsePathPiece s = case String.toLower s of
    "true" -> Just true
    "1" -> Just true
    "false" -> Just false
    "0" -> Just false
    _ -> Nothing

assert :: forall m. Applicative m => Plus m => Boolean -> m Unit
assert true = pure unit
assert false = empty

class ServeAPI :: Row Type -> RowList Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class ServeAPI row list handlers middlewares m | row -> handlers, handlers -> m where
  serveAPI :: Nt m -> Record handlers -> Record middlewares -> Route -> Request -> Server Response

instance ServeAPI row RowList.Nil () middlewares m where
  serveAPI _ _ _ _ _ = pure notFound

instance
  ( ServeRoute t handler middlewares m
  , Row.Cons field t tail row
  , ServeAPI row rest restHandlers middlewares m
  , IsSymbol field
  , Row.Cons field handler restHandlers handlers
  , Row.Lacks field restHandlers
  ) =>
  ServeAPI row (RowList.Cons field t rest) handlers middlewares m where
  serveAPI nt handlers middlewares route req =
    case serveRoute @t route of
      Nothing -> serveAPI @row @rest @restHandlers @middlewares nt (Record.delete (Proxy @field) handlers) middlewares route req
      Just f -> f nt (Record.get (Proxy @field) handlers) middlewares req

notFound :: Response
notFound = Response { body: EmptyResponseBody, status: 404, statusText: "Not Found", headers: Object.empty }

internalServerError :: Response
internalServerError = Response { body: EmptyResponseBody, status: 500, statusText: "Internal Server Error", headers: Object.empty }

badRequest :: Response
badRequest = Response { body: EmptyResponseBody, status: 400, statusText: "Bad Request", headers: Object.empty }

ok :: Response
ok = Response { body: EmptyResponseBody, status: 200, statusText: "OK", headers: Object.empty }

-- | The first argument is used to run an arbitrary monad in handlers. To use the 'Server' monad
-- | instead you can just pass `identity` as the first argument.
run
  :: forall @row list handlers m middlewares
   . RowToList row list
  => ServeAPI row list handlers middlewares m
  => (forall x. m x -> Server x)
  -> { handlers :: Record handlers, middlewares :: Record middlewares }
  -> Request
  -> Aff Bun.Response.Response
run nt { handlers, middlewares } req = do
  Response { body, headers, status, statusText } <-
    map (either identity identity) $ runExceptT $ serveAPI @row @list @handlers (Nt nt) handlers middlewares (routeFromRequest req) req
  let opts = { headers, status, statusText }
  pure $ case body of
    EmptyResponseBody -> Bun.Response.string "" opts
    JsonResponseBody json -> Bun.Response.json json opts
    StringResponseBody string -> Bun.Response.string string opts
