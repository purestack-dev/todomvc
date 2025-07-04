module PureStack.Client
  ( module PureStack.Route
  , client
  -------------------------------------------------------------------------------------------------
  , class ClientAPI
  , clientAPI
  , class ClientRoute
  , clientRoute
  , class EzFetchMethod
  , ezFetchMethod
  , class ToRequest
  , toRequest
  , class FromResponse
  , fromResponse
  , class ToPathPiece
  , toPathPiece
  , class ClientQuery
  , clientQuery
  ) where

import Prelude
import PureStack.Route

import Control.Alternative (class Alternative, empty)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.URL (URL, addQuery)
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.HTTP (Method(..), OptionalFields, fetch)
import Effect.Aff.HTTP.Header as Response
import Effect.Aff.HTTP.Request (Body(..), Credentials(..))
import Effect.Aff.HTTP.Request as HTTP
import Effect.Aff.HTTP.Response (Response)
import Effect.Aff.HTTP.Response as Response
import Foreign (Foreign)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class ClientAPI :: forall k. Row k -> RowList k -> Row Type -> Constraint
class ClientAPI row list req | row -> req, list -> row where
  clientAPI :: URL -> Builder (Record ()) (Record req)

instance ClientAPI () RowList.Nil () where
  clientAPI _ = identity

instance
  ( ClientRoute route req
  , Row.Cons field route tail row
  , Row.Cons field req reqTail reqs
  , ClientAPI tail rest reqTail
  , Row.Lacks field reqTail
  , IsSymbol field
  ) =>
  ClientAPI row (RowList.Cons field route rest) reqs where
  clientAPI baseUrl =
    clientAPI @tail @rest baseUrl >>> Builder.insert (Proxy @field) (clientRoute @route @req baseUrl)

client :: forall @row rowList reqs. RowToList row rowList => ClientAPI row rowList reqs => URL -> Record reqs
client baseUrl = Builder.buildFromScratch (clientAPI @row @rowList baseUrl)

class ClientRoute :: forall k. k -> Type -> Constraint
class ClientRoute route request | route -> request where
  clientRoute :: URL -> request

instance (IsSymbol path, ClientRoute rest req) => ClientRoute (path / rest) req where
  clientRoute = \baseUrl -> clientRoute @rest (baseUrl `URL.addSegment` reflectSymbol (Proxy @path))

else instance (ClientRoute rest req, ClientQuery row rowList, RowToList row rowList) => ClientRoute (Record row / rest) (Record row -> req) where
  clientRoute = \baseUrl rec -> clientRoute @rest (clientQuery @row @rowList rec baseUrl)

else instance ClientRoute rest req => ClientRoute (Middleware middleware / rest) req where
  clientRoute = clientRoute @rest

else instance (ToPathPiece t, ClientRoute rest req) => ClientRoute (t / rest) (t -> req) where
  clientRoute = \baseUrl t -> clientRoute @rest (baseUrl `URL.addSegment` toPathPiece t)

else instance
  ( MethodName (meth req resp)
  , EzFetchMethod (meth req resp)
  , FromResponse resp
  ) =>
  ClientRoute (meth Unit (Headers headers resp)) (Aff resp) where
  clientRoute = \baseUrl -> do
    fetch (ezFetchMethod @(meth req resp)) baseUrl (toRequest unit)
      >>= (liftAff <<< fromResponse)
      >>= case _ of
        Nothing -> empty
        Just x -> pure x

else instance
  ( MethodName (meth req resp)
  , EzFetchMethod (meth req resp)
  , ToRequest req
  , FromResponse resp
  ) =>
  ClientRoute (meth req (Headers headers resp)) (req -> Aff resp) where
  clientRoute = \baseUrl req -> do
    fetch (ezFetchMethod @(meth req resp)) baseUrl (toRequest req)
      >>= (liftAff <<< fromResponse)
      >>= case _ of
        Nothing -> empty
        Just x -> pure x

else instance
  ( MethodName (meth req resp)
  , EzFetchMethod (meth req resp)
  , FromResponse resp
  ) =>
  ClientRoute (meth Unit resp) (Aff resp) where
  clientRoute = \baseUrl -> do
    fetch (ezFetchMethod @(meth req resp)) baseUrl (toRequest unit)
      >>= (liftAff <<< fromResponse)
      >>= case _ of
        Nothing -> empty
        Just x -> pure x

else instance
  ( MethodName (meth req resp)
  , EzFetchMethod (meth req resp)
  , ToRequest req
  , FromResponse resp
  ) =>
  ClientRoute (meth req resp) (req -> Aff resp) where
  clientRoute = \baseUrl req -> do
    fetch (ezFetchMethod @(meth req resp)) baseUrl (toRequest req)
      >>= (liftAff <<< fromResponse)
      >>= case _ of
        Nothing -> empty
        Just x -> pure x
else instance (ClientAPI routes routesList req, RowToList routes routesList) => ClientRoute routes (Record req) where
  clientRoute baseUrl = Builder.buildFromScratch $ clientAPI @routes @routesList baseUrl

class ClientQuery :: Row Type -> RowList Type -> Constraint
class ClientQuery row list where
  clientQuery :: Record row -> URL -> URL

instance ClientQuery row RowList.Nil where
  clientQuery _ url = url

instance
  ( ClientQuery row rest
  , ToPathPiece t
  , IsSymbol field
  , Row.Cons field (Array t) tail row
  ) =>
  ClientQuery row (RowList.Cons field (Array t) rest) where
  clientQuery rec =
    flip addQuery ((reflectSymbol (Proxy @field)) /\ map toPathPiece (Record.get (Proxy @field) rec))
      <<< clientQuery @row @rest rec
else instance
  ( ClientQuery row rest
  , ToPathPiece t
  , IsSymbol field
  , Row.Cons field (Maybe t) tail row
  ) =>
  ClientQuery row (RowList.Cons field (Maybe t) rest) where
  clientQuery rec = case (Record.get (Proxy @field) rec) of
    Just x ->
      flip addQuery ((reflectSymbol (Proxy @field)) /\ toPathPiece x)
        <<< clientQuery @row @rest rec
    Nothing -> identity
else instance
  ( ClientQuery row rest
  , ToPathPiece t
  , IsSymbol field
  , Row.Cons field t tail row
  ) =>
  ClientQuery row (RowList.Cons field t rest) where
  clientQuery rec =
    flip addQuery ((reflectSymbol (Proxy @field)) /\ toPathPiece (Record.get (Proxy @field) rec))
      <<< clientQuery @row @rest rec

class ToPathPiece t where
  toPathPiece :: t -> String

instance ToPathPiece String where
  toPathPiece s = s

instance ToPathPiece Int where
  toPathPiece = show

instance ToPathPiece Number where
  toPathPiece = show

instance ToPathPiece Boolean where
  toPathPiece = show

class ToRequest r where
  toRequest :: r -> Record OptionalFields

instance ToRequest Body where
  toRequest body = { body, credentials: IncludeCredentials, headers: mempty }

instance EncodeJson (Record row) => ToRequest (Record row) where
  toRequest body = { body: HTTP.json (jsonToForeign $ encodeJson body), credentials: IncludeCredentials, headers: mempty }

instance ToRequest Unit where
  toRequest _ = { body: BodyEmpty, credentials: IncludeCredentials, headers: mempty }

instance (ToRequest req, RowToList headers headersList, ToHeaders headers headersList) => ToRequest (Headers (Record headers) req) where
  toRequest (Headers headers req) =
    let
      request = toRequest req
    in
      request
        { headers =
            Response.Headers (Map.fromFoldableWithIndex $ toHeaders @headers @headersList headers)
              <> request.headers

        }

class FromResponse r where
  fromResponse :: Response -> Aff (Maybe r)

instance FromResponse Json where
  fromResponse resp = map (Just <<< foreignToJson) (Response.json resp)

instance DecodeJson x => FromResponse (Array x) where
  fromResponse resp = do
    fromResponse resp <#> case _ of
      Nothing -> Nothing
      Just json ->
        case decodeJson json of
          Right x -> Just x
          Left _ -> Nothing

instance DecodeJson (Record row) => FromResponse (Record row) where
  fromResponse resp = do
    fromResponse resp <#> case _ of
      Nothing -> Nothing
      Just json ->
        case decodeJson json of
          Right x -> Just x
          Left _ -> Nothing

instance FromResponse Unit where
  fromResponse _ = pure $ Just unit

instance FromResponse String where
  fromResponse = map Just <<< Response.text

instance FromResponse ArrayBuffer where
  fromResponse = map Just <<< Response.arrayBuffer

class EzFetchMethod :: HttpMethod -> Constraint
class EzFetchMethod meth where
  ezFetchMethod :: Method

instance EzFetchMethod (GET req resp) where
  ezFetchMethod = GET

instance EzFetchMethod (POST req resp) where
  ezFetchMethod = POST

instance EzFetchMethod (PATCH req resp) where
  ezFetchMethod = PATCH

instance EzFetchMethod (PUT req resp) where
  ezFetchMethod = PUT

instance EzFetchMethod (DELETE req resp) where
  ezFetchMethod = DELETE

jsonToForeign :: Json -> Foreign
jsonToForeign = unsafeCoerce

foreignToJson :: Foreign -> Json
foreignToJson = unsafeCoerce

