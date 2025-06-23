module ProjectM36
  ( withTransaction
  , Connection(..)
  , Transaction
  , define
  , insert
  , query
  , union
  , difference
  , join
  , group
  , ungroup
  , project
  , rename
  , class Relatable
  , attributeTypes
  , encodeRelation
  , class RelVar
  , relVar
  , AtomType(..)
  , Rel
  , rel
  , class Atomable
  , atomType
  , encodeAtom
  , decodeAtom
  , class LiteralRelation
  , lit
  , Expr
  , Attribute
  , class BuildRelation
  , decodeRelationFromObject
  , class AssertNoConflict
  , class GroupRel
  , class DropFields
  , class ReifyFields
  , reifyFields
  , class AssertNoDuplicates
  , class ProjectRel
  , class RenameRel
  , class ReifyRename
  , reifyRename
  ) where

import Prelude
import Prim.Row
import Prim.RowList
import Type.Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, (.:))
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Base64 (decodeBase64, encodeBase64, runBase64)
import Data.Base64 as Base64
import Data.Date (Date)
import Data.DateTime (DateTime(..), date)
import Data.DateTime.ISO (ISO(..), unwrapISO)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL (URL, addSegment)
import Effect.Aff (Aff, bracket, error, throwError)
import Effect.Aff.HTTP (Method(..), fetch)
import Effect.Aff.HTTP.Request as Request
import Effect.Aff.HTTP.Response (Response)
import Effect.Aff.HTTP.Response as Response
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RowList
import Record as Record
import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

data Expr
  = Join Expr Expr
  | Union Expr Expr
  | Project (Set Attribute) Expr
  | Rename (Object Attribute) Expr
  | Difference Expr Expr
  | Group (Set Attribute) Attribute Expr
  | Ungroup Attribute Expr
  | RelationVariable RelVarName
  | Lit Json

instance EncodeJson Expr where
  encodeJson = case _ of
    Lit l -> l
    Join x y -> encodeJson { join: [ x, y ] }
    Union x y -> encodeJson { union: [ x, y ] }
    Project proj expr -> encodeJson { proj: { proj: proj, expr: expr } }
    Rename mapping expr -> encodeJson { rename: { mapping: mapping, expr: expr } }
    Difference x y -> encodeJson { diff: [ x, y ] }
    Group attrs attr expr -> encodeJson { group: { attrs: attrs, attr: attr, expr: expr } }
    Ungroup attr expr -> encodeJson { ungroup: { attr: attr, expr: expr } }
    RelationVariable var -> encodeJson { var: encodeJson var }

type RelVarName = String
type Attribute = String

class RelVar :: forall k. k -> Constraint
class RelVar rel where
  relVar :: String

class LiteralRelation :: Type -> Row Type -> Constraint
class LiteralRelation r fields | r -> fields where
  lit :: r -> Rel fields

instance (RowList.RowToList fields fields', Relatable fields fields') => LiteralRelation (Array (Record fields)) fields where
  lit r = Rel $ Lit $ encodeJson { lit: { attrs: attributeTypes @fields @fields', vals: encodeRelation @fields @fields' <$> r } }

else instance (LiteralRelation (Record fields) fields, Newtype r (Record fields)) => LiteralRelation r fields where
  lit = unwrap >>> lit

rel :: forall @rel fields. Newtype rel (Record fields) => RelVar rel => Rel fields
rel = Rel $ RelationVariable (relVar @rel)

newtype Rel (fields :: Row Type) = Rel Expr

derive newtype instance EncodeJson (Rel fields)

union :: forall fields. Rel fields -> Rel fields -> Rel fields
union (Rel x) (Rel y) = Rel $ Union x y

difference :: forall fields. Rel fields -> Rel fields -> Rel fields
difference (Rel x) (Rel y) = Rel $ Difference x y

join
  :: forall lhs rhs res lhs' rhs' res'
   . Union lhs rhs res
  => RowToList lhs lhs'
  => RowToList rhs rhs'
  => AssertNoConflict lhs' res'
  => AssertNoConflict rhs' res'
  => Nub res res'
  => Rel lhs
  -> Rel rhs
  -> Rel res'
join (Rel lhs) (Rel rhs) = Rel $ Join lhs rhs

class AssertNoConflict :: forall k. RowList k -> Row k -> Constraint
class AssertNoConflict xs ys

instance AssertNoConflict Nil xs
instance (AssertNoConflict ys xs, Row.Cons l y xs' xs) => AssertNoConflict (RowList.Cons l y ys) xs

rename
  :: forall @rename rename' fields fields'
   . RowList.RowToList rename rename'
  => RenameRel rename' fields fields'
  => ReifyRename rename'
  => Rel fields
  -> Rel fields'
rename (Rel x) = Rel $ Rename (reifyRename @rename') x

class ReifyRename :: RowList Symbol -> Constraint
class ReifyRename rename where
  reifyRename :: Object Attribute

instance ReifyRename RowList.Nil where
  reifyRename = Object.empty

instance (ReifyRename rest, IsSymbol from, IsSymbol to) => ReifyRename (RowList.Cons from to rest) where
  reifyRename = Object.insert (reflectSymbol @from Proxy) (reflectSymbol @to Proxy) (reifyRename @rest)

class RenameRel :: forall k. RowList Symbol -> Row k -> Row k -> Constraint
class RenameRel rename fields fields' | rename fields -> fields'

instance RenameRel RowList.Nil fields fields
instance
  ( Row.Cons from t fields' fields
  , Row.Cons to t fields' fields''
  ) =>
  RenameRel (RowList.Cons from to rest) fields fields''

project
  :: forall @projection projection' fields fields'
   . ProjectRel projection' fields fields'
  => RowList.RowToList projection projection'
  => ReifyFields projection'
  => Rel fields
  -> Rel fields'
project (Rel x) = Rel $ Project (reifyFields @projection') x

class ProjectRel :: forall k l. RowList k -> Row l -> Row l -> Constraint
class ProjectRel projection fields fields' | projection fields -> fields'

instance ProjectRel RowList.Nil fields ()
instance
  ( ProjectRel rest fields fields''
  , Row.Cons field value f fields
  , Row.Cons field value fields'' fields'
  ) =>
  ProjectRel (RowList.Cons field v rest) fields fields'

class ReifyFields :: forall k. RowList k -> Constraint
class ReifyFields row where
  reifyFields :: Set Attribute

instance ReifyFields RowList.Nil where
  reifyFields = Set.empty

instance (IsSymbol field, ReifyFields rest) => ReifyFields (RowList.Cons field v rest) where
  reifyFields = Set.insert (reflectSymbol @field Proxy) (reifyFields @rest)

group
  :: forall @group (@field :: Symbol) group' groupped fields fields' fields''
   . RowList.RowToList group group'
  => GroupRel group' fields groupped
  => Row.Cons field (Array (Rel groupped)) fields'' fields'
  => DropFields group' fields fields''
  => ReifyFields group'
  => IsSymbol field
  => Rel fields
  -> Rel fields'
group (Rel x) = Rel $ Group (reifyFields @group') (reflectSymbol @field Proxy) x

class GroupRel :: forall k l. RowList l -> Row k -> Row k -> Constraint
class GroupRel group fields groupped | group fields -> groupped

instance GroupRel RowList.Nil fields ()

instance
  ( GroupRel rest fields fields''
  , Row.Cons field t f fields
  , Row.Cons field t fields'' fields'
  ) =>
  GroupRel (RowList.Cons field u rest) fields fields'

class DropFields :: forall l k. RowList l -> Row k -> Row k -> Constraint
class DropFields drop fields fields' | drop fields -> fields'

instance DropFields RowList.Nil fields fields

instance
  ( DropFields rest fields fields''
  , Row.Cons field t' fields' fields''
  ) =>
  DropFields (RowList.Cons field t rest) fields fields'

ungroup
  :: forall (@field :: Symbol) fields fields' fields'' group group'
   . IsSymbol field
  => Row.Cons field (Array (Record group)) fields'' fields
  => Row.Union group fields'' fields'
  => RowList.RowToList group group'
  => AssertNoDuplicates group' fields''
  => Rel fields
  -> Rel fields'
ungroup (Rel x) = Rel $ Ungroup (reflectSymbol @field Proxy) x

class AssertNoDuplicates :: forall k. RowList k -> Row k -> Constraint
class AssertNoDuplicates xs ys

instance AssertNoDuplicates RowList.Nil ys
instance
  ( AssertNoDuplicates rest ys
  , Row.Lacks field ys
  ) =>
  AssertNoDuplicates (RowList.Cons field t rest) ys

data AtomType
  = IntAtomType
  | IntegerAtomType
  | ScientificAtomType
  | DoubleAtomType
  | TextAtomType
  | DayAtomType
  | DateTimeAtomType
  | ByteStringAtomType
  | BoolAtomType
  | UUIDAtomType
  | RelationAtomType (Array (Attribute /\ AtomType))

instance EncodeJson AtomType where
  encodeJson = case _ of
    IntAtomType -> Argonaut.fromString "int"
    IntegerAtomType -> Argonaut.fromString "integer"
    ScientificAtomType -> Argonaut.fromString "scientific"
    DoubleAtomType -> Argonaut.fromString "double"
    TextAtomType -> Argonaut.fromString "text"
    DayAtomType -> Argonaut.fromString "day"
    DateTimeAtomType -> Argonaut.fromString "datetime"
    ByteStringAtomType -> Argonaut.fromString "bytestring"
    BoolAtomType -> Argonaut.fromString "bool"
    UUIDAtomType -> Argonaut.fromString "uuid"
    RelationAtomType r -> encodeJson r

-- instance DecodeJson AtomType where
--   decodeJson j =
--     ( decodeJson j >>= case _ of
--         "int" -> pure IntegerAtomType
--         "integer" -> pure IntegerAtomType
--         "scientific" -> pure ScientificAtomType
--         "double" -> pure DoubleAtomType
--         "text" -> pure TextAtomType
--         "day" -> pure DayAtomType
--         "datetime" -> pure DateTimeAtomType
--         "bytestring" -> pure ByteStringAtomType
--         "bool" -> pure BoolAtomType
--         "uuid" -> pure UUIDAtomType
--         _ -> Left $ TypeMismatch $ "Unknown atom type " <> stringify j
--     )
--       <|>
--         ( decodeRelation j
--         )

instance WriteForeign AtomType where
  writeImpl = unsafeToForeign <<< encodeJson

class Atomable :: Type -> Constraint
class Atomable a where
  atomType :: AtomType
  encodeAtom :: a -> Json
  decodeAtom :: Json -> Either JsonDecodeError a

instance Atomable Int where
  atomType = IntAtomType
  encodeAtom = encodeJson
  decodeAtom = decodeJson

instance Atomable Number where
  atomType = DoubleAtomType
  encodeAtom = encodeJson
  decodeAtom = decodeJson

instance Atomable String where
  atomType = TextAtomType
  encodeAtom = encodeJson
  decodeAtom = decodeJson

instance Atomable UUID where
  atomType = UUIDAtomType
  encodeAtom (UUID uuid) = encodeJson uuid
  decodeAtom = map UUID <<< decodeJson

instance Atomable Boolean where
  atomType = BoolAtomType
  encodeAtom = encodeJson
  decodeAtom = decodeJson

instance Atomable Date where
  atomType = DayAtomType
  encodeAtom date = encodeJson $ ISO $ DateTime date bottom
  decodeAtom = decodeJson >>> map (unwrapISO >>> date)

instance Atomable DateTime where
  atomType = DateTimeAtomType
  encodeAtom = encodeJson <<< ISO
  decodeAtom = map unwrapISO <<< decodeJson

instance Atomable ArrayBuffer where
  atomType = ByteStringAtomType
  encodeAtom = encodeBase64 >>> runBase64 >>> encodeJson
  decodeAtom j =
    map Base64.fromString (decodeJson j) >>= case _ of
      Just b -> pure $ decodeBase64 b
      Nothing -> Left $ TypeMismatch "Could not decode base64"

instance
  ( RowList.RowToList fields fields'
  , Relatable fields fields'
  , BuildRelation fields fields'
  ) =>
  Atomable (Array (Record fields)) where
  atomType = RelationAtomType (attributeTypes @fields @fields')
  encodeAtom = map (encodeRelation @fields @fields') >>> encodeJson
  decodeAtom = decodeRelationJson >=> traverse (decodeRelationFromObject @fields @fields')

class Relatable :: Row Type -> RowList Type -> Constraint
class Relatable fields fields' where
  attributeTypes :: Array (Attribute /\ AtomType)
  encodeRelation :: Record fields -> Array Json

instance Relatable fields RowList.Nil where
  attributeTypes = []
  encodeRelation _ = []

instance
  ( Atomable t
  , IsSymbol field
  , Relatable fields rest
  , Row.Cons field t tail fields
  ) =>
  Relatable fields (RowList.Cons field t rest) where
  attributeTypes = Array.snoc (attributeTypes @fields @rest) ((reflectSymbol @field Proxy) /\ (atomType @t))
  encodeRelation r = Array.snoc (encodeRelation @fields @rest r) (encodeAtom (Record.get (Proxy @field) r))

decodeRelationJson :: Json -> Either JsonDecodeError (Array (Object Json))
decodeRelationJson = decodeJson >=> \o -> do
  attrs :: Array Attribute <- o .: "attrs"
  (map <<< map) (Object.fromFoldable <<< Array.zip attrs) $ o .: "vals"

class BuildRelation :: Row Type -> RowList Type -> Constraint
class BuildRelation fields fields' | fields' -> fields where
  decodeRelationFromObject :: Object Json -> Either JsonDecodeError (Record fields)

instance BuildRelation () RowList.Nil where
  decodeRelationFromObject _ = pure {}

instance
  ( Row.Cons field r rest fields
  , BuildRelation rest rest'
  , IsSymbol field
  , Atomable r
  , Row.Lacks field rest
  ) =>
  BuildRelation fields (RowList.Cons field t rest') where
  decodeRelationFromObject o = do
    rest :: Record rest <- decodeRelationFromObject @rest @rest' o
    atom <- case Object.lookup (reflectSymbol @field Proxy) o of
      Just js -> decodeAtom js
      Nothing -> Left MissingValue
    pure $ Record.insert (Proxy @field) atom rest

newtype Connection = Connection URL

newtype Transaction a = Transaction (ReaderT { conn :: Connection, sess :: SessionId } Aff a)

derive newtype instance Functor Transaction
derive newtype instance Apply Transaction
derive newtype instance Applicative Transaction
derive newtype instance Bind Transaction
derive newtype instance Monad Transaction

newtype UUID = UUID String

instance DecodeJson UUID where
  decodeJson = map UUID <$> decodeJson

newtype SessionId = SessionId UUID

sessionString :: SessionId -> String
sessionString (SessionId (UUID uuid)) = uuid

withTransaction :: forall a. Connection -> Transaction a -> Aff a
withTransaction conn@(Connection c) (Transaction tr) = do
  bracket
    ( do
        resp <- fetch POST (c `addSegment` "transaction") {}
        Response.guardStatusOk resp
        uuid :: UUID <- json resp >>= \j -> case decodeJson j of
          Left _ -> throwError $ error "Could not decode transaction session UUID"
          Right sess -> pure sess
        pure $ SessionId uuid
    )
    ( \sess ->
        fetch DELETE (c `addSegment` "transaction" `addSegment` sessionString sess) {}
          >>= Response.guardStatusOk
    )
    ( \sess -> do
        a <- runReaderT tr { conn: conn, sess: sess }
        fetch PUT (c `addSegment` "transaction" `addSegment` sessionString sess) {}
          >>= Response.guardStatusOk
        pure a
    )

json :: Response -> Aff Json
json resp = unsafeFromForeign <$> Response.json resp

define
  :: forall @rel fields fields'
   . RowList.RowToList fields fields'
  => Newtype rel (Record fields)
  => Relatable fields fields'
  => RelVar rel
  => Transaction Unit
define = Transaction $ ReaderT $ \{ conn: Connection c, sess } -> do
  fetch POST (c `addSegment` "transaction" `addSegment` sessionString sess)
    { body: Request.json { define: { name: relVar @rel, attrs: Object.fromFoldable (attributeTypes @fields @fields') } } }
    >>= Response.guardStatusOk
  pure unit

insert
  :: forall @rel fields
   . RelVar rel
  => Newtype rel (Record fields)
  => Rel fields
  -> Transaction Unit
insert (Rel r) = Transaction $ ReaderT $ \{ conn: Connection c, sess } -> do
  fetch POST (c `addSegment` "transaction" `addSegment` sessionString sess)
    { body: Request.json { insert: { name: relVar @rel, rel: jsonToForeign (encodeJson r) } } }
    >>= Response.guardStatusOk
  pure unit

query
  :: forall fields fields'
   . RowList.RowToList fields fields'
  => BuildRelation fields fields'
  => Rel fields
  -> Transaction (Array (Record fields))
query (Rel r) = Transaction $ ReaderT $ \{ conn: Connection c, sess } -> do
  resp <- fetch POST (c `addSegment` "transaction" `addSegment` sessionString sess)
    { body: Request.json { query: jsonToForeign (encodeJson r) } }
  Response.guardStatusOk resp
  j <- json resp
  case decodeRelation @fields @fields' j of
    Right rec -> pure rec
    Left err -> throwError $ error $ show err

decodeRelation
  :: forall @fields @fields'
   . BuildRelation fields fields'
  => Json
  -> Either JsonDecodeError (Array (Record fields))
decodeRelation j = decodeRelationJson j >>= traverse (decodeRelationFromObject @fields @fields')

jsonToForeign :: Json -> Foreign
jsonToForeign = unsafeCoerce

