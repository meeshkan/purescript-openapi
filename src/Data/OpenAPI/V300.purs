module Data.OpenAPI.V300 (T_Info,
 _Schema,
 _Contact,
 Server(..),
 _Discriminator,
 OAIMap,
 _AdditionalBoolean,
 T_Server,
 JSON,
 T_ServerVariable,
 T_PathItem,
 _HTTPSecurityScheme,
 T_ClientCredentialsFlow,
 _ItemsAsTuple,
 Components(..),
 _Encoding,
 _Ref,
 Example(..),
 _AdditionalSchema,
 T_XML,
 Contact(..),
 T_OAuth2SecurityScheme,
 ImplicitOAuthFlow(..),
 _OpenIdConnectSS,
 T_APIKeySecurityScheme,
 T_PasswordOAuthFlow,
 _APIKeySecurityScheme,
 T_HTTPSecurityScheme,
 APIKeySecurityScheme(..),
 Parameter(..),
 _AuthorizationCodeOAuthFlow,
 _OAuthFlows,
 T_License,
 _OAuth2SecurityScheme,
 OpenAPIObject(..),
 MediaType(..),
 _Operation,
 _Info,
 Discriminator(..),
 T_Reference,
 XML(..),
 T_AuthorizationCodeOAuthFlow,
 _AdditionalReference,
 OpenIdConnectSecurityScheme(..),
 Reference(..),
 BooleanInt,
 _SingleItemReference,
 _ReferenceSS,
 T_Parameter,
 _Parameter,
 T_MediaType,
 Header(..),
 Info(..),
 SecuritySchema(..),
 ServerVariable(..),
 _AnInt,
 ExternalDocumentation(..),
 _PasswordOAuthFlow,
 Additionals(..),
 _PathItem,
 _HTTPSS,
 _Server,
 _License,
 _OpenIdConnectSecurityScheme,
 AuthorizationCodeOAuthFlow(..),
 _RequestBody,
 PathItem(..),
 HTTPSecurityScheme(..),
 T_ImplicitOAuthFlow,
 _Link,
 ReferenceOr,
 _ClientCredentialsFlow,
 T_Example,
 _StringSS,
 _Response,
 _XML,
 License(..),
 Operation(..),
 Response(..),
 Encoding(..),
 _Components,
 T_Schema,
 T_Discriminator,
 T_Response,
 OAuthFlows(..),
 _MediaType,
 Link(..),
 T_ExternalDocumentation,
 _ServerVariable,
 T_Operation,
 _OpenAPIObject,
 ClientCredentialsFlow(..),
 _SingleItem,
 _RealDeal,
 PasswordOAuthFlow(..),
 T_Encoding,
 Schema(..),
 _Tag,
 Items(..),
 T_RequestBody,
 _APIKeySS,
 _ExternalDocumentation,
 _Reference,
 T_Header,
 OAuth2SecurityScheme(..),
 T_Components,
 Tag(..),
 _Example,
 T_OpenIdConnectSecurityScheme,
 T_Contact,
 _Header,
 T_Tag,
 _OAuth2SS,
 T_Link,
 RequestBody(..),
 _ImplicitOAuthFlow,
 T_OAuthFlows,
 _ABoolean,
 T_OpenAPIObject) where

import Prelude
import Control.Alt ((<|>))
import Data.Array (findIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, null)
import Data.String.Utils (startsWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, isNull, readArray, readBoolean, readInt, readNumber, readString)
import Foreign.Index (readProp)
import Foreign.Keys (keys)
import Foreign.Object as FO
import Simple.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)


newtype OAIMap b = OAIMap (Map.Map String b)
derive newtype instance eqOAIMap :: (Eq a) => Eq (OAIMap a)

instance readForeignOAIMap :: (ReadForeign a) => ReadForeign (OAIMap a) where
  readImpl f = do
    v <- (readImpl f)
    pure (OAIMap $ Map.fromFoldable ((FO.toUnfoldable $ v) :: (Array (Tuple String a))))


oaiMapToObject :: ∀ a. (WriteForeign a) => OAIMap a -> FO.Object a
oaiMapToObject (OAIMap f) = FO.fromFoldable ((Map.toUnfoldable f) :: (Array (Tuple String a)))

instance writeForeignOAIMap :: (WriteForeign a) => WriteForeign (OAIMap a) where
  writeImpl f = writeImpl (oaiMapToObject f)

data JSON = JObject (OAIMap JSON) | JArray (Array JSON) | JString String | JBoolean Boolean | JNumber Number | JNull
derive instance genericJSON :: Generic JSON _

instance readForeignJSON :: ReadForeign JSON where
  readImpl f = if (isNull f) then pure JNull else (readNumber f >>= pure <<< JNumber) <|> (readBoolean f >>= pure <<< JBoolean) <|> (readString f >>= pure <<< JString) <|> (readArray f >>= sequence <<< map readImpl >>= pure <<< JArray) <|> (readImpl f >>= pure <<< JObject)

instance writeForeignJSON :: WriteForeign JSON where
  writeImpl (JObject j) = writeImpl $ oaiMapToObject j
  writeImpl (JArray j) = writeImpl j
  writeImpl (JBoolean j) = writeImpl j
  writeImpl (JNumber j) = writeImpl j
  writeImpl (JString j) = writeImpl j
  writeImpl JNull = writeImpl (null :: Nullable Int) -- chose Int at random

instance eqJSON :: Eq JSON where
  eq a b = genericEq a b
hack :: ∀ a b c. (a -> c) -> (a -> b -> c)
hack o = (\x -> (\y -> o x))

xify :: Foreign -> F (Maybe (OAIMap JSON))
xify f = do
  (OAIMap asMap) <- (readImpl f) :: (F (OAIMap JSON))
  pure  $ Just (OAIMap (Map.filterKeys (startsWith "x-") asMap))
isRef :: Foreign -> F Boolean
isRef f = keys f >>= pure <<< (/=) Nothing <<< findIndex ((==) "$ref")

data ReferenceOr a = Ref Reference | RealDeal a
derive instance eqReferenceOr :: (Eq a) => Eq (ReferenceOr a)


_Ref ∷
  ∀ a.
  Tuple
    ( Reference → ReferenceOr a
    )
    ( ReferenceOr a →
      Maybe Reference
    )
_Ref =
  Tuple Ref
    ( case _ of
        Ref a → Just a
        _ → Nothing
    )

_RealDeal ∷ ∀ a.
  Tuple
    ( a → ReferenceOr a
    )
    ( ReferenceOr a →
      Maybe a
    )
_RealDeal =
  Tuple RealDeal
    ( case _ of
        RealDeal a → Just a
        _ → Nothing
    )
instance readForeignReferenceOr :: (ReadForeign a) => ReadForeign (ReferenceOr a) where
  readImpl f = do
    iref <- isRef f
    if iref then Ref <$> readImpl f else RealDeal <$> readImpl f

instance writeForeignReferenceOr :: (WriteForeign a) => WriteForeign (ReferenceOr a) where
  writeImpl (Ref t) = writeImpl t
  writeImpl (RealDeal t) = writeImpl t
data BooleanInt = ABoolean Boolean | AnInt Int
derive instance genericBooleanInt  :: Generic BooleanInt  _
instance eqBooleanInt :: Eq BooleanInt where
  eq = genericEq
instance readForeignBooleanInt ::  ReadForeign BooleanInt where
  readImpl f = (readBoolean f >>= pure <<< ABoolean) <|> (readInt f >>= pure <<< AnInt)

instance writeForeignBooleanInt :: WriteForeign BooleanInt where
  writeImpl (ABoolean b) = writeImpl b
  writeImpl (AnInt i) = writeImpl i

_ABoolean ∷
  Tuple
    ( Boolean → BooleanInt
    )
    ( BooleanInt →
      Maybe Boolean
    )
_ABoolean =
  Tuple ABoolean
    ( case _ of
        ABoolean a → Just a
        _ → Nothing
    )

_AnInt ∷
  Tuple
    ( Int → BooleanInt
    )
    ( BooleanInt →
      Maybe Int
    )
_AnInt =
  Tuple AnInt
    ( case _ of
        AnInt a → Just a
        _ → Nothing
    )
-- |OpenAPIObject
type T_OpenAPIObject = {_openapi :: String, _info :: Info, _paths :: ((OAIMap PathItem)), _externalDocs :: (Maybe ExternalDocumentation), _servers :: (Maybe ((Array Server))), _security :: (Maybe ((Array (OAIMap ((Array String)))))), _tags :: (Maybe ((Array Tag))), _components :: (Maybe Components), _x :: (Maybe ((OAIMap JSON)))}

newtype OpenAPIObject = OpenAPIObject T_OpenAPIObject

instance eqOpenAPIObject :: Eq OpenAPIObject where
  eq (OpenAPIObject f0) (OpenAPIObject f1) = (f0._openapi == f1._openapi) && (f0._info == f1._info) && (f0._paths == f1._paths) && (f0._externalDocs == f1._externalDocs) && (f0._servers == f1._servers) && (f0._security == f1._security) && (f0._tags == f1._tags) && (f0._components == f1._components) && (f0._x == f1._x)


instance writeForeignOpenAPIObject :: WriteForeign OpenAPIObject where
  writeImpl (OpenAPIObject f) =
    writeImpl $ FO.fromFoldable ([Tuple "openapi" (writeImpl f._openapi)] <> [Tuple "info" (writeImpl f._info)] <> [Tuple "paths" (writeImpl f._paths)] <> (maybe [] (\x -> [Tuple "externalDocs" (writeImpl x)]) f._externalDocs) <> (maybe [] (\x -> [Tuple "servers" (writeImpl x)]) f._servers) <> (maybe [] (\x -> [Tuple "security" (writeImpl x)]) f._security) <> (maybe [] (\x -> [Tuple "tags" (writeImpl x)]) f._tags) <> (maybe [] (\x -> [Tuple "components" (writeImpl x)]) f._components) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignOpenAPIObject :: ReadForeign OpenAPIObject where
  readImpl f = do
    _openapi <- readProp "openapi" f >>= readImpl
    _info <- readProp "info" f >>= readImpl
    _paths <- readProp "paths" f >>= readImpl
    _externalDocs <- (readProp "externalDocs" f >>= readImpl) <|> (pure Nothing)
    _servers <- (readProp "servers" f >>= readImpl) <|> (pure Nothing)
    _security <- (readProp "security" f >>= readImpl) <|> (pure Nothing)
    _tags <- (readProp "tags" f >>= readImpl) <|> (pure Nothing)
    _components <- (readProp "components" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ OpenAPIObject {_openapi,_info,_paths,_externalDocs,_servers,_security,_tags,_components,_x}
_OpenAPIObject ∷
  Tuple
    ( T_OpenAPIObject → OpenAPIObject
    )
    ( OpenAPIObject →
      Maybe T_OpenAPIObject
    )
_OpenAPIObject =
  Tuple OpenAPIObject
    ( case _ of
        OpenAPIObject a → Just a
    )

-- |ExternalDocumentation
type T_ExternalDocumentation = {_url :: String, _description :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype ExternalDocumentation = ExternalDocumentation T_ExternalDocumentation

instance eqExternalDocumentation :: Eq ExternalDocumentation where
  eq (ExternalDocumentation f0) (ExternalDocumentation f1) = (f0._url == f1._url) && (f0._description == f1._description) && (f0._x == f1._x)


instance writeForeignExternalDocumentation :: WriteForeign ExternalDocumentation where
  writeImpl (ExternalDocumentation f) =
    writeImpl $ FO.fromFoldable ([Tuple "url" (writeImpl f._url)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignExternalDocumentation :: ReadForeign ExternalDocumentation where
  readImpl f = do
    _url <- readProp "url" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ ExternalDocumentation {_url,_description,_x}
_ExternalDocumentation ∷
  Tuple
    ( T_ExternalDocumentation → ExternalDocumentation
    )
    ( ExternalDocumentation →
      Maybe T_ExternalDocumentation
    )
_ExternalDocumentation =
  Tuple ExternalDocumentation
    ( case _ of
        ExternalDocumentation a → Just a
    )

-- |Tag
type T_Tag = {_name :: String, _description :: (Maybe String), _externalDocs :: (Maybe ExternalDocumentation), _x :: (Maybe ((OAIMap JSON)))}

newtype Tag = Tag T_Tag

instance eqTag :: Eq Tag where
  eq (Tag f0) (Tag f1) = (f0._name == f1._name) && (f0._description == f1._description) && (f0._externalDocs == f1._externalDocs) && (f0._x == f1._x)


instance writeForeignTag :: WriteForeign Tag where
  writeImpl (Tag f) =
    writeImpl $ FO.fromFoldable ([Tuple "name" (writeImpl f._name)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "externalDocs" (writeImpl x)]) f._externalDocs) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignTag :: ReadForeign Tag where
  readImpl f = do
    _name <- readProp "name" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _externalDocs <- (readProp "externalDocs" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Tag {_name,_description,_externalDocs,_x}
_Tag ∷
  Tuple
    ( T_Tag → Tag
    )
    ( Tag →
      Maybe T_Tag
    )
_Tag =
  Tuple Tag
    ( case _ of
        Tag a → Just a
    )

-- |Info
type T_Info = {_title :: String, _version :: String, _description :: (Maybe String), _termsOfService :: (Maybe String), _contact :: (Maybe Contact), _license :: (Maybe License), _x :: (Maybe ((OAIMap JSON)))}

newtype Info = Info T_Info

instance eqInfo :: Eq Info where
  eq (Info f0) (Info f1) = (f0._title == f1._title) && (f0._version == f1._version) && (f0._description == f1._description) && (f0._termsOfService == f1._termsOfService) && (f0._contact == f1._contact) && (f0._license == f1._license) && (f0._x == f1._x)


instance writeForeignInfo :: WriteForeign Info where
  writeImpl (Info f) =
    writeImpl $ FO.fromFoldable ([Tuple "title" (writeImpl f._title)] <> [Tuple "version" (writeImpl f._version)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "termsOfService" (writeImpl x)]) f._termsOfService) <> (maybe [] (\x -> [Tuple "contact" (writeImpl x)]) f._contact) <> (maybe [] (\x -> [Tuple "license" (writeImpl x)]) f._license) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignInfo :: ReadForeign Info where
  readImpl f = do
    _title <- readProp "title" f >>= readImpl
    _version <- readProp "version" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _termsOfService <- (readProp "termsOfService" f >>= readImpl) <|> (pure Nothing)
    _contact <- (readProp "contact" f >>= readImpl) <|> (pure Nothing)
    _license <- (readProp "license" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Info {_title,_version,_description,_termsOfService,_contact,_license,_x}
_Info ∷
  Tuple
    ( T_Info → Info
    )
    ( Info →
      Maybe T_Info
    )
_Info =
  Tuple Info
    ( case _ of
        Info a → Just a
    )

-- |PathItem
type T_PathItem = {_summary :: (Maybe String), _description :: (Maybe String), _servers :: (Maybe ((Array Server))), _parameters :: (Maybe ((Array (ReferenceOr Parameter)))), _get :: (Maybe Operation), _put :: (Maybe Operation), _post :: (Maybe Operation), _delete :: (Maybe Operation), _options :: (Maybe Operation), _head :: (Maybe Operation), _patch :: (Maybe Operation), _trace :: (Maybe Operation), _ref :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype PathItem = PathItem T_PathItem

instance eqPathItem :: Eq PathItem where
  eq (PathItem f0) (PathItem f1) = (f0._summary == f1._summary) && (f0._description == f1._description) && (f0._servers == f1._servers) && (f0._parameters == f1._parameters) && (f0._get == f1._get) && (f0._put == f1._put) && (f0._post == f1._post) && (f0._delete == f1._delete) && (f0._options == f1._options) && (f0._head == f1._head) && (f0._patch == f1._patch) && (f0._trace == f1._trace) && (f0._ref == f1._ref) && (f0._x == f1._x)


instance writeForeignPathItem :: WriteForeign PathItem where
  writeImpl (PathItem f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "summary" (writeImpl x)]) f._summary) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "servers" (writeImpl x)]) f._servers) <> (maybe [] (\x -> [Tuple "parameters" (writeImpl x)]) f._parameters) <> (maybe [] (\x -> [Tuple "get" (writeImpl x)]) f._get) <> (maybe [] (\x -> [Tuple "put" (writeImpl x)]) f._put) <> (maybe [] (\x -> [Tuple "post" (writeImpl x)]) f._post) <> (maybe [] (\x -> [Tuple "delete" (writeImpl x)]) f._delete) <> (maybe [] (\x -> [Tuple "options" (writeImpl x)]) f._options) <> (maybe [] (\x -> [Tuple "head" (writeImpl x)]) f._head) <> (maybe [] (\x -> [Tuple "patch" (writeImpl x)]) f._patch) <> (maybe [] (\x -> [Tuple "trace" (writeImpl x)]) f._trace) <> (maybe [] (\x -> [Tuple "$ref" (writeImpl x)]) f._ref) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignPathItem :: ReadForeign PathItem where
  readImpl f = do
    _summary <- (readProp "summary" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _servers <- (readProp "servers" f >>= readImpl) <|> (pure Nothing)
    _parameters <- (readProp "parameters" f >>= readImpl) <|> (pure Nothing)
    _get <- (readProp "get" f >>= readImpl) <|> (pure Nothing)
    _put <- (readProp "put" f >>= readImpl) <|> (pure Nothing)
    _post <- (readProp "post" f >>= readImpl) <|> (pure Nothing)
    _delete <- (readProp "delete" f >>= readImpl) <|> (pure Nothing)
    _options <- (readProp "options" f >>= readImpl) <|> (pure Nothing)
    _head <- (readProp "head" f >>= readImpl) <|> (pure Nothing)
    _patch <- (readProp "patch" f >>= readImpl) <|> (pure Nothing)
    _trace <- (readProp "trace" f >>= readImpl) <|> (pure Nothing)
    _ref <- (readProp "$ref" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ PathItem {_summary,_description,_servers,_parameters,_get,_put,_post,_delete,_options,_head,_patch,_trace,_ref,_x}
_PathItem ∷
  Tuple
    ( T_PathItem → PathItem
    )
    ( PathItem →
      Maybe T_PathItem
    )
_PathItem =
  Tuple PathItem
    ( case _ of
        PathItem a → Just a
    )

-- |Server
type T_Server = {_url :: String, _description :: (Maybe String), _variables :: (Maybe ((OAIMap ServerVariable))), _x :: (Maybe ((OAIMap JSON)))}

newtype Server = Server T_Server

instance eqServer :: Eq Server where
  eq (Server f0) (Server f1) = (f0._url == f1._url) && (f0._description == f1._description) && (f0._variables == f1._variables) && (f0._x == f1._x)


instance writeForeignServer :: WriteForeign Server where
  writeImpl (Server f) =
    writeImpl $ FO.fromFoldable ([Tuple "url" (writeImpl f._url)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "variables" (writeImpl x)]) f._variables) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignServer :: ReadForeign Server where
  readImpl f = do
    _url <- readProp "url" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _variables <- (readProp "variables" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Server {_url,_description,_variables,_x}
_Server ∷
  Tuple
    ( T_Server → Server
    )
    ( Server →
      Maybe T_Server
    )
_Server =
  Tuple Server
    ( case _ of
        Server a → Just a
    )

-- |Components
type T_Components = {_schemas :: (Maybe ((OAIMap ((ReferenceOr Schema))))), _responses :: (Maybe ((OAIMap ((ReferenceOr Response))))), _parameters :: (Maybe ((OAIMap ((ReferenceOr Parameter))))), _examples :: (Maybe ((OAIMap ((ReferenceOr Example))))), _requestBodies :: (Maybe ((OAIMap ((ReferenceOr RequestBody))))), _headers :: (Maybe ((OAIMap ((ReferenceOr Header))))), _securitySchemes :: (Maybe ((OAIMap SecuritySchema))), _links :: (Maybe ((OAIMap ((ReferenceOr Link))))), _callbacks :: (Maybe ((OAIMap ((ReferenceOr ((OAIMap PathItem))))))), _x :: (Maybe ((OAIMap JSON)))}

newtype Components = Components T_Components

instance eqComponents :: Eq Components where
  eq (Components f0) (Components f1) = (f0._schemas == f1._schemas) && (f0._responses == f1._responses) && (f0._parameters == f1._parameters) && (f0._examples == f1._examples) && (f0._requestBodies == f1._requestBodies) && (f0._headers == f1._headers) && (f0._securitySchemes == f1._securitySchemes) && (f0._links == f1._links) && (f0._callbacks == f1._callbacks) && (f0._x == f1._x)


instance writeForeignComponents :: WriteForeign Components where
  writeImpl (Components f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "schemas" (writeImpl x)]) f._schemas) <> (maybe [] (\x -> [Tuple "responses" (writeImpl x)]) f._responses) <> (maybe [] (\x -> [Tuple "parameters" (writeImpl x)]) f._parameters) <> (maybe [] (\x -> [Tuple "examples" (writeImpl x)]) f._examples) <> (maybe [] (\x -> [Tuple "requestBodies" (writeImpl x)]) f._requestBodies) <> (maybe [] (\x -> [Tuple "headers" (writeImpl x)]) f._headers) <> (maybe [] (\x -> [Tuple "securitySchemes" (writeImpl x)]) f._securitySchemes) <> (maybe [] (\x -> [Tuple "links" (writeImpl x)]) f._links) <> (maybe [] (\x -> [Tuple "callbacks" (writeImpl x)]) f._callbacks) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignComponents :: ReadForeign Components where
  readImpl f = do
    _schemas <- (readProp "schemas" f >>= readImpl) <|> (pure Nothing)
    _responses <- (readProp "responses" f >>= readImpl) <|> (pure Nothing)
    _parameters <- (readProp "parameters" f >>= readImpl) <|> (pure Nothing)
    _examples <- (readProp "examples" f >>= readImpl) <|> (pure Nothing)
    _requestBodies <- (readProp "requestBodies" f >>= readImpl) <|> (pure Nothing)
    _headers <- (readProp "headers" f >>= readImpl) <|> (pure Nothing)
    _securitySchemes <- (readProp "securitySchemes" f >>= readImpl) <|> (pure Nothing)
    _links <- (readProp "links" f >>= readImpl) <|> (pure Nothing)
    _callbacks <- (readProp "callbacks" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Components {_schemas,_responses,_parameters,_examples,_requestBodies,_headers,_securitySchemes,_links,_callbacks,_x}
_Components ∷
  Tuple
    ( T_Components → Components
    )
    ( Components →
      Maybe T_Components
    )
_Components =
  Tuple Components
    ( case _ of
        Components a → Just a
    )

data SecuritySchema = APIKeySS APIKeySecurityScheme | 
      HTTPSS HTTPSecurityScheme |
      OAuth2SS OAuth2SecurityScheme | 
      OpenIdConnectSS OpenIdConnectSecurityScheme | 
      StringSS String | 
      ReferenceSS Reference

derive instance genericSecuritySchema  :: Generic SecuritySchema  _
instance eqSecuritySchema :: Eq SecuritySchema where
  eq = genericEq
securitySchemaFromObject :: Foreign -> F SecuritySchema
securitySchemaFromObject f = do
    tp <- readProp "type" f >>= readString
    case tp of
      "apiKey" -> APIKeySS <$> readImpl f
      "http" -> HTTPSS <$> readImpl f
      "oauth2" -> OAuth2SS <$> readImpl f
      "openIdConnect" -> OpenIdConnectSS <$> readImpl f
      _ -> ReferenceSS <$> readImpl f

securitySchemaFromString :: Foreign -> F SecuritySchema
securitySchemaFromString f = readString f >>= pure <<< StringSS

instance readForeignSecuritySchema :: ReadForeign SecuritySchema where
  readImpl f = securitySchemaFromObject f <|> securitySchemaFromString f

instance writeForeignSecuritySchema :: WriteForeign SecuritySchema where
  writeImpl (APIKeySS r) = writeImpl r
  writeImpl (HTTPSS r) = writeImpl r
  writeImpl (OAuth2SS r) = writeImpl r
  writeImpl (OpenIdConnectSS r) = writeImpl r
  writeImpl (StringSS r) = writeImpl r
  writeImpl (ReferenceSS r) = writeImpl r

_APIKeySS ∷
  Tuple
    ( APIKeySecurityScheme → SecuritySchema
    )
    ( SecuritySchema →
      Maybe APIKeySecurityScheme
    )
_APIKeySS =
  Tuple APIKeySS
    ( case _ of
        APIKeySS a → Just a
        _ → Nothing
    )

_HTTPSS ∷
  Tuple
    ( HTTPSecurityScheme → SecuritySchema
    )
    ( SecuritySchema →
      Maybe HTTPSecurityScheme
    )
_HTTPSS =
  Tuple HTTPSS
    ( case _ of
        HTTPSS a → Just a
        _ → Nothing
    )

_OAuth2SS ∷
  Tuple
    ( OAuth2SecurityScheme → SecuritySchema
    )
    ( SecuritySchema →
      Maybe OAuth2SecurityScheme
    )
_OAuth2SS =
  Tuple OAuth2SS
    ( case _ of
        OAuth2SS a → Just a
        _ → Nothing
    )

_OpenIdConnectSS ∷
  Tuple
    ( OpenIdConnectSecurityScheme → SecuritySchema
    )
    ( SecuritySchema →
      Maybe OpenIdConnectSecurityScheme
    )
_OpenIdConnectSS =
  Tuple OpenIdConnectSS
    ( case _ of
        OpenIdConnectSS a → Just a
        _ → Nothing
    )

_StringSS ∷
  Tuple
    ( String → SecuritySchema
    )
    ( SecuritySchema →
      Maybe String
    )
_StringSS =
  Tuple StringSS
    ( case _ of
        StringSS a → Just a
        _ → Nothing
    )

_ReferenceSS ∷
  Tuple
    ( Reference → SecuritySchema
    )
    ( SecuritySchema →
      Maybe Reference
    )
_ReferenceSS =
  Tuple ReferenceSS
    ( case _ of
        ReferenceSS a → Just a
        _ → Nothing
    )

-- |Response
type T_Response = {_description :: String, _headers :: (Maybe ((OAIMap ((ReferenceOr Header))))), _content :: (Maybe ((OAIMap MediaType))), _links :: (Maybe ((OAIMap ((ReferenceOr Link))))), _x :: (Maybe ((OAIMap JSON)))}

newtype Response = Response T_Response

instance eqResponse :: Eq Response where
  eq (Response f0) (Response f1) = (f0._description == f1._description) && (f0._headers == f1._headers) && (f0._content == f1._content) && (f0._links == f1._links) && (f0._x == f1._x)


instance writeForeignResponse :: WriteForeign Response where
  writeImpl (Response f) =
    writeImpl $ FO.fromFoldable ([Tuple "description" (writeImpl f._description)] <> (maybe [] (\x -> [Tuple "headers" (writeImpl x)]) f._headers) <> (maybe [] (\x -> [Tuple "content" (writeImpl x)]) f._content) <> (maybe [] (\x -> [Tuple "links" (writeImpl x)]) f._links) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignResponse :: ReadForeign Response where
  readImpl f = do
    _description <- readProp "description" f >>= readImpl
    _headers <- (readProp "headers" f >>= readImpl) <|> (pure Nothing)
    _content <- (readProp "content" f >>= readImpl) <|> (pure Nothing)
    _links <- (readProp "links" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Response {_description,_headers,_content,_links,_x}
_Response ∷
  Tuple
    ( T_Response → Response
    )
    ( Response →
      Maybe T_Response
    )
_Response =
  Tuple Response
    ( case _ of
        Response a → Just a
    )

-- |RequestBody
type T_RequestBody = {_content :: ((OAIMap MediaType)), _description :: (Maybe String), _required :: (Maybe Boolean), _x :: (Maybe ((OAIMap JSON)))}

newtype RequestBody = RequestBody T_RequestBody

instance eqRequestBody :: Eq RequestBody where
  eq (RequestBody f0) (RequestBody f1) = (f0._content == f1._content) && (f0._description == f1._description) && (f0._required == f1._required) && (f0._x == f1._x)


instance writeForeignRequestBody :: WriteForeign RequestBody where
  writeImpl (RequestBody f) =
    writeImpl $ FO.fromFoldable ([Tuple "content" (writeImpl f._content)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "required" (writeImpl x)]) f._required) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignRequestBody :: ReadForeign RequestBody where
  readImpl f = do
    _content <- readProp "content" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _required <- (readProp "required" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ RequestBody {_content,_description,_required,_x}
_RequestBody ∷
  Tuple
    ( T_RequestBody → RequestBody
    )
    ( RequestBody →
      Maybe T_RequestBody
    )
_RequestBody =
  Tuple RequestBody
    ( case _ of
        RequestBody a → Just a
    )

-- |Link
type T_Link = {_operationId :: (Maybe String), _operationRef :: (Maybe String), _parameters :: (Maybe ((OAIMap JSON))), _requestBody :: (Maybe JSON), _description :: (Maybe String), _server :: (Maybe Server), _x :: (Maybe ((OAIMap JSON)))}

newtype Link = Link T_Link

instance eqLink :: Eq Link where
  eq (Link f0) (Link f1) = (f0._operationId == f1._operationId) && (f0._operationRef == f1._operationRef) && (f0._parameters == f1._parameters) && (f0._requestBody == f1._requestBody) && (f0._description == f1._description) && (f0._server == f1._server) && (f0._x == f1._x)


instance writeForeignLink :: WriteForeign Link where
  writeImpl (Link f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "operationId" (writeImpl x)]) f._operationId) <> (maybe [] (\x -> [Tuple "operationRef" (writeImpl x)]) f._operationRef) <> (maybe [] (\x -> [Tuple "parameters" (writeImpl x)]) f._parameters) <> (maybe [] (\x -> [Tuple "requestBody" (writeImpl x)]) f._requestBody) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "server" (writeImpl x)]) f._server) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignLink :: ReadForeign Link where
  readImpl f = do
    _operationId <- (readProp "operationId" f >>= readImpl) <|> (pure Nothing)
    _operationRef <- (readProp "operationRef" f >>= readImpl) <|> (pure Nothing)
    _parameters <- (readProp "parameters" f >>= readImpl) <|> (pure Nothing)
    _requestBody <- (readProp "requestBody" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _server <- (readProp "server" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Link {_operationId,_operationRef,_parameters,_requestBody,_description,_server,_x}
_Link ∷
  Tuple
    ( T_Link → Link
    )
    ( Link →
      Maybe T_Link
    )
_Link =
  Tuple Link
    ( case _ of
        Link a → Just a
    )

-- |Example
type T_Example = {_summary :: (Maybe String), _description :: (Maybe String), _value :: (Maybe JSON), _externalValue :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype Example = Example T_Example

instance eqExample :: Eq Example where
  eq (Example f0) (Example f1) = (f0._summary == f1._summary) && (f0._description == f1._description) && (f0._value == f1._value) && (f0._externalValue == f1._externalValue) && (f0._x == f1._x)


instance writeForeignExample :: WriteForeign Example where
  writeImpl (Example f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "summary" (writeImpl x)]) f._summary) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "value" (writeImpl x)]) f._value) <> (maybe [] (\x -> [Tuple "externalValue" (writeImpl x)]) f._externalValue) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignExample :: ReadForeign Example where
  readImpl f = do
    _summary <- (readProp "summary" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _value <- (readProp "value" f >>= readImpl) <|> (pure Nothing)
    _externalValue <- (readProp "externalValue" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Example {_summary,_description,_value,_externalValue,_x}
_Example ∷
  Tuple
    ( T_Example → Example
    )
    ( Example →
      Maybe T_Example
    )
_Example =
  Tuple Example
    ( case _ of
        Example a → Just a
    )

-- |Parameter
type T_Parameter = {_name :: String, _in :: String, _description :: (Maybe String), _required :: (Maybe Boolean), _deprecated :: (Maybe Boolean), _allowEmptyValue :: (Maybe Boolean), _style :: (Maybe String), _explode :: (Maybe Boolean), _allowReserved :: (Maybe Boolean), _schema :: (Maybe (ReferenceOr Schema)), _content :: (Maybe ((OAIMap MediaType))), _example :: (Maybe JSON), _examples :: (Maybe ((OAIMap ((ReferenceOr Example))))), _x :: (Maybe ((OAIMap JSON)))}

newtype Parameter = Parameter T_Parameter

instance eqParameter :: Eq Parameter where
  eq (Parameter f0) (Parameter f1) = (f0._name == f1._name) && (f0._in == f1._in) && (f0._description == f1._description) && (f0._required == f1._required) && (f0._deprecated == f1._deprecated) && (f0._allowEmptyValue == f1._allowEmptyValue) && (f0._style == f1._style) && (f0._explode == f1._explode) && (f0._allowReserved == f1._allowReserved) && (f0._schema == f1._schema) && (f0._content == f1._content) && (f0._example == f1._example) && (f0._examples == f1._examples) && (f0._x == f1._x)


instance writeForeignParameter :: WriteForeign Parameter where
  writeImpl (Parameter f) =
    writeImpl $ FO.fromFoldable ([Tuple "name" (writeImpl f._name)] <> [Tuple "in" (writeImpl f._in)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "required" (writeImpl x)]) f._required) <> (maybe [] (\x -> [Tuple "deprecated" (writeImpl x)]) f._deprecated) <> (maybe [] (\x -> [Tuple "allowEmptyValue" (writeImpl x)]) f._allowEmptyValue) <> (maybe [] (\x -> [Tuple "style" (writeImpl x)]) f._style) <> (maybe [] (\x -> [Tuple "explode" (writeImpl x)]) f._explode) <> (maybe [] (\x -> [Tuple "allowReserved" (writeImpl x)]) f._allowReserved) <> (maybe [] (\x -> [Tuple "schema" (writeImpl x)]) f._schema) <> (maybe [] (\x -> [Tuple "content" (writeImpl x)]) f._content) <> (maybe [] (\x -> [Tuple "example" (writeImpl x)]) f._example) <> (maybe [] (\x -> [Tuple "examples" (writeImpl x)]) f._examples) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignParameter :: ReadForeign Parameter where
  readImpl f = do
    _name <- readProp "name" f >>= readImpl
    _in <- readProp "in" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _required <- (readProp "required" f >>= readImpl) <|> (pure Nothing)
    _deprecated <- (readProp "deprecated" f >>= readImpl) <|> (pure Nothing)
    _allowEmptyValue <- (readProp "allowEmptyValue" f >>= readImpl) <|> (pure Nothing)
    _style <- (readProp "style" f >>= readImpl) <|> (pure Nothing)
    _explode <- (readProp "explode" f >>= readImpl) <|> (pure Nothing)
    _allowReserved <- (readProp "allowReserved" f >>= readImpl) <|> (pure Nothing)
    _schema <- (readProp "schema" f >>= readImpl) <|> (pure Nothing)
    _content <- (readProp "content" f >>= readImpl) <|> (pure Nothing)
    _example <- (readProp "example" f >>= readImpl) <|> (pure Nothing)
    _examples <- (readProp "examples" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Parameter {_name,_in,_description,_required,_deprecated,_allowEmptyValue,_style,_explode,_allowReserved,_schema,_content,_example,_examples,_x}
_Parameter ∷
  Tuple
    ( T_Parameter → Parameter
    )
    ( Parameter →
      Maybe T_Parameter
    )
_Parameter =
  Tuple Parameter
    ( case _ of
        Parameter a → Just a
    )

-- |Schema
type T_Schema = {_title :: (Maybe String), _multipleOf :: (Maybe Number), _maximum :: (Maybe Number), _exclusiveMaximum :: (Maybe BooleanInt), _minimum :: (Maybe Number), _exclusiveMinimum :: (Maybe BooleanInt), _maxLength :: (Maybe Int), _minLength :: (Maybe Int), _pattern :: (Maybe String), _maxItems :: (Maybe Int), _minItems :: (Maybe Int), _uniqueItems :: (Maybe Boolean), _maxProperties :: (Maybe Int), _minProperties :: (Maybe Int), _required :: (Maybe ((Array String))), _enum :: (Maybe ((Array JSON))), _allOf :: (Maybe ((Array (ReferenceOr Schema)))), _oneOf :: (Maybe ((Array (ReferenceOr Schema)))), _anyOf :: (Maybe ((Array (ReferenceOr Schema)))), _items :: (Maybe Items), _properties :: (Maybe ((OAIMap ((ReferenceOr Schema))))), _additionalProperties :: (Maybe Additionals), _description :: (Maybe String), _default :: (Maybe JSON), _nullable :: (Maybe Boolean), _discriminator :: (Maybe Discriminator), _readOnly :: (Maybe Boolean), _writeOnly :: (Maybe Boolean), _example :: (Maybe JSON), _externalDocs :: (Maybe ExternalDocumentation), _deprecated :: (Maybe Boolean), _xml :: (Maybe XML), _format :: (Maybe String), _type :: (Maybe String), _not :: (Maybe (ReferenceOr Schema)), _x :: (Maybe ((OAIMap JSON)))}

newtype Schema = Schema T_Schema

instance eqSchema :: Eq Schema where
  eq (Schema f0) (Schema f1) = (f0._title == f1._title) && (f0._multipleOf == f1._multipleOf) && (f0._maximum == f1._maximum) && (f0._exclusiveMaximum == f1._exclusiveMaximum) && (f0._minimum == f1._minimum) && (f0._exclusiveMinimum == f1._exclusiveMinimum) && (f0._maxLength == f1._maxLength) && (f0._minLength == f1._minLength) && (f0._pattern == f1._pattern) && (f0._maxItems == f1._maxItems) && (f0._minItems == f1._minItems) && (f0._uniqueItems == f1._uniqueItems) && (f0._maxProperties == f1._maxProperties) && (f0._minProperties == f1._minProperties) && (f0._required == f1._required) && (f0._enum == f1._enum) && (f0._allOf == f1._allOf) && (f0._oneOf == f1._oneOf) && (f0._anyOf == f1._anyOf) && (f0._items == f1._items) && (f0._properties == f1._properties) && (f0._additionalProperties == f1._additionalProperties) && (f0._description == f1._description) && (f0._default == f1._default) && (f0._nullable == f1._nullable) && (f0._discriminator == f1._discriminator) && (f0._readOnly == f1._readOnly) && (f0._writeOnly == f1._writeOnly) && (f0._example == f1._example) && (f0._externalDocs == f1._externalDocs) && (f0._deprecated == f1._deprecated) && (f0._xml == f1._xml) && (f0._format == f1._format) && (f0._type == f1._type) && (f0._not == f1._not) && (f0._x == f1._x)


instance writeForeignSchema :: WriteForeign Schema where
  writeImpl (Schema f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "title" (writeImpl x)]) f._title) <> (maybe [] (\x -> [Tuple "multipleOf" (writeImpl x)]) f._multipleOf) <> (maybe [] (\x -> [Tuple "maximum" (writeImpl x)]) f._maximum) <> (maybe [] (\x -> [Tuple "exclusiveMaximum" (writeImpl x)]) f._exclusiveMaximum) <> (maybe [] (\x -> [Tuple "minimum" (writeImpl x)]) f._minimum) <> (maybe [] (\x -> [Tuple "exclusiveMinimum" (writeImpl x)]) f._exclusiveMinimum) <> (maybe [] (\x -> [Tuple "maxLength" (writeImpl x)]) f._maxLength) <> (maybe [] (\x -> [Tuple "minLength" (writeImpl x)]) f._minLength) <> (maybe [] (\x -> [Tuple "pattern" (writeImpl x)]) f._pattern) <> (maybe [] (\x -> [Tuple "maxItems" (writeImpl x)]) f._maxItems) <> (maybe [] (\x -> [Tuple "minItems" (writeImpl x)]) f._minItems) <> (maybe [] (\x -> [Tuple "uniqueItems" (writeImpl x)]) f._uniqueItems) <> (maybe [] (\x -> [Tuple "maxProperties" (writeImpl x)]) f._maxProperties) <> (maybe [] (\x -> [Tuple "minProperties" (writeImpl x)]) f._minProperties) <> (maybe [] (\x -> [Tuple "required" (writeImpl x)]) f._required) <> (maybe [] (\x -> [Tuple "enum" (writeImpl x)]) f._enum) <> (maybe [] (\x -> [Tuple "allOf" (writeImpl x)]) f._allOf) <> (maybe [] (\x -> [Tuple "oneOf" (writeImpl x)]) f._oneOf) <> (maybe [] (\x -> [Tuple "anyOf" (writeImpl x)]) f._anyOf) <> (maybe [] (\x -> [Tuple "items" (writeImpl x)]) f._items) <> (maybe [] (\x -> [Tuple "properties" (writeImpl x)]) f._properties) <> (maybe [] (\x -> [Tuple "additionalProperties" (writeImpl x)]) f._additionalProperties) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "default" (writeImpl x)]) f._default) <> (maybe [] (\x -> [Tuple "nullable" (writeImpl x)]) f._nullable) <> (maybe [] (\x -> [Tuple "discriminator" (writeImpl x)]) f._discriminator) <> (maybe [] (\x -> [Tuple "readOnly" (writeImpl x)]) f._readOnly) <> (maybe [] (\x -> [Tuple "writeOnly" (writeImpl x)]) f._writeOnly) <> (maybe [] (\x -> [Tuple "example" (writeImpl x)]) f._example) <> (maybe [] (\x -> [Tuple "externalDocs" (writeImpl x)]) f._externalDocs) <> (maybe [] (\x -> [Tuple "deprecated" (writeImpl x)]) f._deprecated) <> (maybe [] (\x -> [Tuple "xml" (writeImpl x)]) f._xml) <> (maybe [] (\x -> [Tuple "format" (writeImpl x)]) f._format) <> (maybe [] (\x -> [Tuple "type" (writeImpl x)]) f._type) <> (maybe [] (\x -> [Tuple "not" (writeImpl x)]) f._not) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignSchema :: ReadForeign Schema where
  readImpl f = do
    _title <- (readProp "title" f >>= readImpl) <|> (pure Nothing)
    _multipleOf <- (readProp "multipleOf" f >>= readImpl) <|> (pure Nothing)
    _maximum <- (readProp "maximum" f >>= readImpl) <|> (pure Nothing)
    _exclusiveMaximum <- (readProp "exclusiveMaximum" f >>= readImpl) <|> (pure Nothing)
    _minimum <- (readProp "minimum" f >>= readImpl) <|> (pure Nothing)
    _exclusiveMinimum <- (readProp "exclusiveMinimum" f >>= readImpl) <|> (pure Nothing)
    _maxLength <- (readProp "maxLength" f >>= readImpl) <|> (pure Nothing)
    _minLength <- (readProp "minLength" f >>= readImpl) <|> (pure Nothing)
    _pattern <- (readProp "pattern" f >>= readImpl) <|> (pure Nothing)
    _maxItems <- (readProp "maxItems" f >>= readImpl) <|> (pure Nothing)
    _minItems <- (readProp "minItems" f >>= readImpl) <|> (pure Nothing)
    _uniqueItems <- (readProp "uniqueItems" f >>= readImpl) <|> (pure Nothing)
    _maxProperties <- (readProp "maxProperties" f >>= readImpl) <|> (pure Nothing)
    _minProperties <- (readProp "minProperties" f >>= readImpl) <|> (pure Nothing)
    _required <- (readProp "required" f >>= readImpl) <|> (pure Nothing)
    _enum <- (readProp "enum" f >>= readImpl) <|> (pure Nothing)
    _allOf <- (readProp "allOf" f >>= readImpl) <|> (pure Nothing)
    _oneOf <- (readProp "oneOf" f >>= readImpl) <|> (pure Nothing)
    _anyOf <- (readProp "anyOf" f >>= readImpl) <|> (pure Nothing)
    _items <- (readProp "items" f >>= readImpl) <|> (pure Nothing)
    _properties <- (readProp "properties" f >>= readImpl) <|> (pure Nothing)
    _additionalProperties <- (readProp "additionalProperties" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _default <- (readProp "default" f >>= readImpl) <|> (pure Nothing)
    _nullable <- (readProp "nullable" f >>= readImpl) <|> (pure Nothing)
    _discriminator <- (readProp "discriminator" f >>= readImpl) <|> (pure Nothing)
    _readOnly <- (readProp "readOnly" f >>= readImpl) <|> (pure Nothing)
    _writeOnly <- (readProp "writeOnly" f >>= readImpl) <|> (pure Nothing)
    _example <- (readProp "example" f >>= readImpl) <|> (pure Nothing)
    _externalDocs <- (readProp "externalDocs" f >>= readImpl) <|> (pure Nothing)
    _deprecated <- (readProp "deprecated" f >>= readImpl) <|> (pure Nothing)
    _xml <- (readProp "xml" f >>= readImpl) <|> (pure Nothing)
    _format <- (readProp "format" f >>= readImpl) <|> (pure Nothing)
    _type <- (readProp "type" f >>= readImpl) <|> (pure Nothing)
    _not <- (readProp "not" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Schema {_title,_multipleOf,_maximum,_exclusiveMaximum,_minimum,_exclusiveMinimum,_maxLength,_minLength,_pattern,_maxItems,_minItems,_uniqueItems,_maxProperties,_minProperties,_required,_enum,_allOf,_oneOf,_anyOf,_items,_properties,_additionalProperties,_description,_default,_nullable,_discriminator,_readOnly,_writeOnly,_example,_externalDocs,_deprecated,_xml,_format,_type,_not,_x}
_Schema ∷
  Tuple
    ( T_Schema → Schema
    )
    ( Schema →
      Maybe T_Schema
    )
_Schema =
  Tuple Schema
    ( case _ of
        Schema a → Just a
    )

-- |License
type T_License = {_name :: String, _url :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype License = License T_License

instance eqLicense :: Eq License where
  eq (License f0) (License f1) = (f0._name == f1._name) && (f0._url == f1._url) && (f0._x == f1._x)


instance writeForeignLicense :: WriteForeign License where
  writeImpl (License f) =
    writeImpl $ FO.fromFoldable ([Tuple "name" (writeImpl f._name)] <> (maybe [] (\x -> [Tuple "url" (writeImpl x)]) f._url) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignLicense :: ReadForeign License where
  readImpl f = do
    _name <- readProp "name" f >>= readImpl
    _url <- (readProp "url" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ License {_name,_url,_x}
_License ∷
  Tuple
    ( T_License → License
    )
    ( License →
      Maybe T_License
    )
_License =
  Tuple License
    ( case _ of
        License a → Just a
    )

-- |Operation
type T_Operation = {_responses :: ((OAIMap ((ReferenceOr Response)))), _tags :: (Maybe ((Array String))), _summary :: (Maybe String), _description :: (Maybe String), _externalDocs :: (Maybe ExternalDocumentation), _operationId :: (Maybe String), _parameters :: (Maybe ((Array (ReferenceOr Parameter)))), _requestBody :: (Maybe (ReferenceOr RequestBody)), _callbacks :: (Maybe ((OAIMap ((ReferenceOr ((OAIMap PathItem))))))), _deprecated :: (Maybe Boolean), _security :: (Maybe ((Array (OAIMap ((Array String)))))), _servers :: (Maybe ((Array Server))), _x :: (Maybe ((OAIMap JSON)))}

newtype Operation = Operation T_Operation

instance eqOperation :: Eq Operation where
  eq (Operation f0) (Operation f1) = (f0._responses == f1._responses) && (f0._tags == f1._tags) && (f0._summary == f1._summary) && (f0._description == f1._description) && (f0._externalDocs == f1._externalDocs) && (f0._operationId == f1._operationId) && (f0._parameters == f1._parameters) && (f0._requestBody == f1._requestBody) && (f0._callbacks == f1._callbacks) && (f0._deprecated == f1._deprecated) && (f0._security == f1._security) && (f0._servers == f1._servers) && (f0._x == f1._x)


instance writeForeignOperation :: WriteForeign Operation where
  writeImpl (Operation f) =
    writeImpl $ FO.fromFoldable ([Tuple "responses" (writeImpl f._responses)] <> (maybe [] (\x -> [Tuple "tags" (writeImpl x)]) f._tags) <> (maybe [] (\x -> [Tuple "summary" (writeImpl x)]) f._summary) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "externalDocs" (writeImpl x)]) f._externalDocs) <> (maybe [] (\x -> [Tuple "operationId" (writeImpl x)]) f._operationId) <> (maybe [] (\x -> [Tuple "parameters" (writeImpl x)]) f._parameters) <> (maybe [] (\x -> [Tuple "requestBody" (writeImpl x)]) f._requestBody) <> (maybe [] (\x -> [Tuple "callbacks" (writeImpl x)]) f._callbacks) <> (maybe [] (\x -> [Tuple "deprecated" (writeImpl x)]) f._deprecated) <> (maybe [] (\x -> [Tuple "security" (writeImpl x)]) f._security) <> (maybe [] (\x -> [Tuple "servers" (writeImpl x)]) f._servers) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignOperation :: ReadForeign Operation where
  readImpl f = do
    _responses <- readProp "responses" f >>= readImpl
    _tags <- (readProp "tags" f >>= readImpl) <|> (pure Nothing)
    _summary <- (readProp "summary" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _externalDocs <- (readProp "externalDocs" f >>= readImpl) <|> (pure Nothing)
    _operationId <- (readProp "operationId" f >>= readImpl) <|> (pure Nothing)
    _parameters <- (readProp "parameters" f >>= readImpl) <|> (pure Nothing)
    _requestBody <- (readProp "requestBody" f >>= readImpl) <|> (pure Nothing)
    _callbacks <- (readProp "callbacks" f >>= readImpl) <|> (pure Nothing)
    _deprecated <- (readProp "deprecated" f >>= readImpl) <|> (pure Nothing)
    _security <- (readProp "security" f >>= readImpl) <|> (pure Nothing)
    _servers <- (readProp "servers" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Operation {_responses,_tags,_summary,_description,_externalDocs,_operationId,_parameters,_requestBody,_callbacks,_deprecated,_security,_servers,_x}
_Operation ∷
  Tuple
    ( T_Operation → Operation
    )
    ( Operation →
      Maybe T_Operation
    )
_Operation =
  Tuple Operation
    ( case _ of
        Operation a → Just a
    )

-- |Contact
type T_Contact = {_name :: (Maybe String), _url :: (Maybe String), _email :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype Contact = Contact T_Contact

instance eqContact :: Eq Contact where
  eq (Contact f0) (Contact f1) = (f0._name == f1._name) && (f0._url == f1._url) && (f0._email == f1._email) && (f0._x == f1._x)


instance writeForeignContact :: WriteForeign Contact where
  writeImpl (Contact f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "name" (writeImpl x)]) f._name) <> (maybe [] (\x -> [Tuple "url" (writeImpl x)]) f._url) <> (maybe [] (\x -> [Tuple "email" (writeImpl x)]) f._email) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignContact :: ReadForeign Contact where
  readImpl f = do
    _name <- (readProp "name" f >>= readImpl) <|> (pure Nothing)
    _url <- (readProp "url" f >>= readImpl) <|> (pure Nothing)
    _email <- (readProp "email" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Contact {_name,_url,_email,_x}
_Contact ∷
  Tuple
    ( T_Contact → Contact
    )
    ( Contact →
      Maybe T_Contact
    )
_Contact =
  Tuple Contact
    ( case _ of
        Contact a → Just a
    )

-- |Header
type T_Header = {_description :: (Maybe String), _required :: (Maybe Boolean), _deprecated :: (Maybe Boolean), _allowEmptyValue :: (Maybe Boolean), _style :: (Maybe String), _explode :: (Maybe Boolean), _allowReserved :: (Maybe Boolean), _schema :: (Maybe (ReferenceOr Schema)), _content :: (Maybe ((OAIMap MediaType))), _example :: (Maybe JSON), _examples :: (Maybe ((OAIMap ((ReferenceOr Example))))), _x :: (Maybe ((OAIMap JSON)))}

newtype Header = Header T_Header

instance eqHeader :: Eq Header where
  eq (Header f0) (Header f1) = (f0._description == f1._description) && (f0._required == f1._required) && (f0._deprecated == f1._deprecated) && (f0._allowEmptyValue == f1._allowEmptyValue) && (f0._style == f1._style) && (f0._explode == f1._explode) && (f0._allowReserved == f1._allowReserved) && (f0._schema == f1._schema) && (f0._content == f1._content) && (f0._example == f1._example) && (f0._examples == f1._examples) && (f0._x == f1._x)


instance writeForeignHeader :: WriteForeign Header where
  writeImpl (Header f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\x -> [Tuple "required" (writeImpl x)]) f._required) <> (maybe [] (\x -> [Tuple "deprecated" (writeImpl x)]) f._deprecated) <> (maybe [] (\x -> [Tuple "allowEmptyValue" (writeImpl x)]) f._allowEmptyValue) <> (maybe [] (\x -> [Tuple "style" (writeImpl x)]) f._style) <> (maybe [] (\x -> [Tuple "explode" (writeImpl x)]) f._explode) <> (maybe [] (\x -> [Tuple "allowReserved" (writeImpl x)]) f._allowReserved) <> (maybe [] (\x -> [Tuple "schema" (writeImpl x)]) f._schema) <> (maybe [] (\x -> [Tuple "content" (writeImpl x)]) f._content) <> (maybe [] (\x -> [Tuple "example" (writeImpl x)]) f._example) <> (maybe [] (\x -> [Tuple "examples" (writeImpl x)]) f._examples) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignHeader :: ReadForeign Header where
  readImpl f = do
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _required <- (readProp "required" f >>= readImpl) <|> (pure Nothing)
    _deprecated <- (readProp "deprecated" f >>= readImpl) <|> (pure Nothing)
    _allowEmptyValue <- (readProp "allowEmptyValue" f >>= readImpl) <|> (pure Nothing)
    _style <- (readProp "style" f >>= readImpl) <|> (pure Nothing)
    _explode <- (readProp "explode" f >>= readImpl) <|> (pure Nothing)
    _allowReserved <- (readProp "allowReserved" f >>= readImpl) <|> (pure Nothing)
    _schema <- (readProp "schema" f >>= readImpl) <|> (pure Nothing)
    _content <- (readProp "content" f >>= readImpl) <|> (pure Nothing)
    _example <- (readProp "example" f >>= readImpl) <|> (pure Nothing)
    _examples <- (readProp "examples" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Header {_description,_required,_deprecated,_allowEmptyValue,_style,_explode,_allowReserved,_schema,_content,_example,_examples,_x}
_Header ∷
  Tuple
    ( T_Header → Header
    )
    ( Header →
      Maybe T_Header
    )
_Header =
  Tuple Header
    ( case _ of
        Header a → Just a
    )

-- |ServerVariable
type T_ServerVariable = {_default :: String, _enum :: (Maybe ((Array String))), _description :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype ServerVariable = ServerVariable T_ServerVariable

instance eqServerVariable :: Eq ServerVariable where
  eq (ServerVariable f0) (ServerVariable f1) = (f0._default == f1._default) && (f0._enum == f1._enum) && (f0._description == f1._description) && (f0._x == f1._x)


instance writeForeignServerVariable :: WriteForeign ServerVariable where
  writeImpl (ServerVariable f) =
    writeImpl $ FO.fromFoldable ([Tuple "default" (writeImpl f._default)] <> (maybe [] (\x -> [Tuple "enum" (writeImpl x)]) f._enum) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignServerVariable :: ReadForeign ServerVariable where
  readImpl f = do
    _default <- readProp "default" f >>= readImpl
    _enum <- (readProp "enum" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ ServerVariable {_default,_enum,_description,_x}
_ServerVariable ∷
  Tuple
    ( T_ServerVariable → ServerVariable
    )
    ( ServerVariable →
      Maybe T_ServerVariable
    )
_ServerVariable =
  Tuple ServerVariable
    ( case _ of
        ServerVariable a → Just a
    )

-- |XML
type T_XML = {_name :: (Maybe String), _namespace :: (Maybe String), _prefix :: (Maybe String), _attribute :: (Maybe Boolean), _wrapped :: (Maybe Boolean), _x :: (Maybe ((OAIMap JSON)))}

newtype XML = XML T_XML

instance eqXML :: Eq XML where
  eq (XML f0) (XML f1) = (f0._name == f1._name) && (f0._namespace == f1._namespace) && (f0._prefix == f1._prefix) && (f0._attribute == f1._attribute) && (f0._wrapped == f1._wrapped) && (f0._x == f1._x)


instance writeForeignXML :: WriteForeign XML where
  writeImpl (XML f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "name" (writeImpl x)]) f._name) <> (maybe [] (\x -> [Tuple "namespace" (writeImpl x)]) f._namespace) <> (maybe [] (\x -> [Tuple "prefix" (writeImpl x)]) f._prefix) <> (maybe [] (\x -> [Tuple "attribute" (writeImpl x)]) f._attribute) <> (maybe [] (\x -> [Tuple "wrapped" (writeImpl x)]) f._wrapped) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignXML :: ReadForeign XML where
  readImpl f = do
    _name <- (readProp "name" f >>= readImpl) <|> (pure Nothing)
    _namespace <- (readProp "namespace" f >>= readImpl) <|> (pure Nothing)
    _prefix <- (readProp "prefix" f >>= readImpl) <|> (pure Nothing)
    _attribute <- (readProp "attribute" f >>= readImpl) <|> (pure Nothing)
    _wrapped <- (readProp "wrapped" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ XML {_name,_namespace,_prefix,_attribute,_wrapped,_x}
_XML ∷
  Tuple
    ( T_XML → XML
    )
    ( XML →
      Maybe T_XML
    )
_XML =
  Tuple XML
    ( case _ of
        XML a → Just a
    )

-- |APIKeySecurityScheme
type T_APIKeySecurityScheme = {_name :: String, _type :: String, _in :: String, _description :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype APIKeySecurityScheme = APIKeySecurityScheme T_APIKeySecurityScheme

instance eqAPIKeySecurityScheme :: Eq APIKeySecurityScheme where
  eq (APIKeySecurityScheme f0) (APIKeySecurityScheme f1) = (f0._name == f1._name) && (f0._type == f1._type) && (f0._in == f1._in) && (f0._description == f1._description) && (f0._x == f1._x)


instance writeForeignAPIKeySecurityScheme :: WriteForeign APIKeySecurityScheme where
  writeImpl (APIKeySecurityScheme f) =
    writeImpl $ FO.fromFoldable ([Tuple "name" (writeImpl f._name)] <> [Tuple "type" (writeImpl f._type)] <> [Tuple "in" (writeImpl f._in)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignAPIKeySecurityScheme :: ReadForeign APIKeySecurityScheme where
  readImpl f = do
    _name <- readProp "name" f >>= readImpl
    _type <- readProp "type" f >>= readImpl
    _in <- readProp "in" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ APIKeySecurityScheme {_name,_type,_in,_description,_x}
_APIKeySecurityScheme ∷
  Tuple
    ( T_APIKeySecurityScheme → APIKeySecurityScheme
    )
    ( APIKeySecurityScheme →
      Maybe T_APIKeySecurityScheme
    )
_APIKeySecurityScheme =
  Tuple APIKeySecurityScheme
    ( case _ of
        APIKeySecurityScheme a → Just a
    )

-- |OpenIdConnectSecurityScheme
type T_OpenIdConnectSecurityScheme = {_type :: String, _openIdConnectUrl :: String, _description :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype OpenIdConnectSecurityScheme = OpenIdConnectSecurityScheme T_OpenIdConnectSecurityScheme

instance eqOpenIdConnectSecurityScheme :: Eq OpenIdConnectSecurityScheme where
  eq (OpenIdConnectSecurityScheme f0) (OpenIdConnectSecurityScheme f1) = (f0._type == f1._type) && (f0._openIdConnectUrl == f1._openIdConnectUrl) && (f0._description == f1._description) && (f0._x == f1._x)


instance writeForeignOpenIdConnectSecurityScheme :: WriteForeign OpenIdConnectSecurityScheme where
  writeImpl (OpenIdConnectSecurityScheme f) =
    writeImpl $ FO.fromFoldable ([Tuple "type" (writeImpl f._type)] <> [Tuple "openIdConnectUrl" (writeImpl f._openIdConnectUrl)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignOpenIdConnectSecurityScheme :: ReadForeign OpenIdConnectSecurityScheme where
  readImpl f = do
    _type <- readProp "type" f >>= readImpl
    _openIdConnectUrl <- readProp "openIdConnectUrl" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ OpenIdConnectSecurityScheme {_type,_openIdConnectUrl,_description,_x}
_OpenIdConnectSecurityScheme ∷
  Tuple
    ( T_OpenIdConnectSecurityScheme → OpenIdConnectSecurityScheme
    )
    ( OpenIdConnectSecurityScheme →
      Maybe T_OpenIdConnectSecurityScheme
    )
_OpenIdConnectSecurityScheme =
  Tuple OpenIdConnectSecurityScheme
    ( case _ of
        OpenIdConnectSecurityScheme a → Just a
    )


data Items = ItemsAsTuple (Array (ReferenceOr Schema)) | SingleItem Schema | SingleItemReference Reference
derive instance genericItems  :: Generic Items  _
instance eqItems :: Eq Items where
  eq = genericEq

readForeignItemsAsObject :: Foreign -> F Items
readForeignItemsAsObject f = do
  iref <- isRef f
  if iref then SingleItemReference <$> readImpl f else SingleItem <$> readImpl f

readForeignItemsAsArray :: Foreign -> F Items
readForeignItemsAsArray f = readArray f >>= sequence <<< map readImpl >>= pure <<< ItemsAsTuple


instance readForeignItems :: ReadForeign Items where
  readImpl f = readForeignItemsAsObject f <|> readForeignItemsAsArray f

instance writeForeignItems :: WriteForeign Items where
  writeImpl (ItemsAsTuple t) = writeImpl t
  writeImpl (SingleItem i) = writeImpl i
  writeImpl (SingleItemReference r) = writeImpl r


_ItemsAsTuple ∷
  Tuple
    ( (Array (ReferenceOr Schema)) → Items
    )
    ( Items →
      Maybe (Array (ReferenceOr Schema))
    )
_ItemsAsTuple =
  Tuple ItemsAsTuple
    ( case _ of
        ItemsAsTuple a → Just a
        _ → Nothing
    )

_SingleItem ∷
  Tuple
    ( Schema → Items
    )
    ( Items →
      Maybe Schema
    )
_SingleItem =
  Tuple SingleItem
    ( case _ of
        SingleItem a → Just a
        _ → Nothing
    )

_SingleItemReference ∷
  Tuple
    ( Reference → Items
    )
    ( Items →
      Maybe Reference
    )
_SingleItemReference =
  Tuple SingleItemReference
    ( case _ of
        SingleItemReference a → Just a
        _ → Nothing
    )

-- |OAuth2SecurityScheme
type T_OAuth2SecurityScheme = {_flows :: OAuthFlows, _type :: String, _description :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype OAuth2SecurityScheme = OAuth2SecurityScheme T_OAuth2SecurityScheme

instance eqOAuth2SecurityScheme :: Eq OAuth2SecurityScheme where
  eq (OAuth2SecurityScheme f0) (OAuth2SecurityScheme f1) = (f0._flows == f1._flows) && (f0._type == f1._type) && (f0._description == f1._description) && (f0._x == f1._x)


instance writeForeignOAuth2SecurityScheme :: WriteForeign OAuth2SecurityScheme where
  writeImpl (OAuth2SecurityScheme f) =
    writeImpl $ FO.fromFoldable ([Tuple "flows" (writeImpl f._flows)] <> [Tuple "type" (writeImpl f._type)] <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignOAuth2SecurityScheme :: ReadForeign OAuth2SecurityScheme where
  readImpl f = do
    _flows <- readProp "flows" f >>= readImpl
    _type <- readProp "type" f >>= readImpl
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ OAuth2SecurityScheme {_flows,_type,_description,_x}
_OAuth2SecurityScheme ∷
  Tuple
    ( T_OAuth2SecurityScheme → OAuth2SecurityScheme
    )
    ( OAuth2SecurityScheme →
      Maybe T_OAuth2SecurityScheme
    )
_OAuth2SecurityScheme =
  Tuple OAuth2SecurityScheme
    ( case _ of
        OAuth2SecurityScheme a → Just a
    )

-- |Reference
type T_Reference = {_ref :: String, _x :: (Maybe ((OAIMap JSON)))}

newtype Reference = Reference T_Reference

instance eqReference :: Eq Reference where
  eq (Reference f0) (Reference f1) = (f0._ref == f1._ref) && (f0._x == f1._x)


instance writeForeignReference :: WriteForeign Reference where
  writeImpl (Reference f) =
    writeImpl $ FO.fromFoldable ([Tuple "$ref" (writeImpl f._ref)] <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignReference :: ReadForeign Reference where
  readImpl f = do
    _ref <- readProp "$ref" f >>= readImpl
    _x <- xify f
    pure $ Reference {_ref,_x}
_Reference ∷
  Tuple
    ( T_Reference → Reference
    )
    ( Reference →
      Maybe T_Reference
    )
_Reference =
  Tuple Reference
    ( case _ of
        Reference a → Just a
    )

-- |HTTPSecurityScheme
type T_HTTPSecurityScheme = {_type :: String, _scheme :: String, _bearerFormat :: (Maybe String), _description :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype HTTPSecurityScheme = HTTPSecurityScheme T_HTTPSecurityScheme

instance eqHTTPSecurityScheme :: Eq HTTPSecurityScheme where
  eq (HTTPSecurityScheme f0) (HTTPSecurityScheme f1) = (f0._type == f1._type) && (f0._scheme == f1._scheme) && (f0._bearerFormat == f1._bearerFormat) && (f0._description == f1._description) && (f0._x == f1._x)


instance writeForeignHTTPSecurityScheme :: WriteForeign HTTPSecurityScheme where
  writeImpl (HTTPSecurityScheme f) =
    writeImpl $ FO.fromFoldable ([Tuple "type" (writeImpl f._type)] <> [Tuple "scheme" (writeImpl f._scheme)] <> (maybe [] (\x -> [Tuple "bearerFormat" (writeImpl x)]) f._bearerFormat) <> (maybe [] (\x -> [Tuple "description" (writeImpl x)]) f._description) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignHTTPSecurityScheme :: ReadForeign HTTPSecurityScheme where
  readImpl f = do
    _type <- readProp "type" f >>= readImpl
    _scheme <- readProp "scheme" f >>= readImpl
    _bearerFormat <- (readProp "bearerFormat" f >>= readImpl) <|> (pure Nothing)
    _description <- (readProp "description" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ HTTPSecurityScheme {_type,_scheme,_bearerFormat,_description,_x}
_HTTPSecurityScheme ∷
  Tuple
    ( T_HTTPSecurityScheme → HTTPSecurityScheme
    )
    ( HTTPSecurityScheme →
      Maybe T_HTTPSecurityScheme
    )
_HTTPSecurityScheme =
  Tuple HTTPSecurityScheme
    ( case _ of
        HTTPSecurityScheme a → Just a
    )

-- |Discriminator
type T_Discriminator = {_propertyName :: String, _mapping :: (Maybe ((OAIMap String))), _x :: (Maybe ((OAIMap JSON)))}

newtype Discriminator = Discriminator T_Discriminator

instance eqDiscriminator :: Eq Discriminator where
  eq (Discriminator f0) (Discriminator f1) = (f0._propertyName == f1._propertyName) && (f0._mapping == f1._mapping) && (f0._x == f1._x)


instance writeForeignDiscriminator :: WriteForeign Discriminator where
  writeImpl (Discriminator f) =
    writeImpl $ FO.fromFoldable ([Tuple "propertyName" (writeImpl f._propertyName)] <> (maybe [] (\x -> [Tuple "mapping" (writeImpl x)]) f._mapping) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignDiscriminator :: ReadForeign Discriminator where
  readImpl f = do
    _propertyName <- readProp "propertyName" f >>= readImpl
    _mapping <- (readProp "mapping" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Discriminator {_propertyName,_mapping,_x}
_Discriminator ∷
  Tuple
    ( T_Discriminator → Discriminator
    )
    ( Discriminator →
      Maybe T_Discriminator
    )
_Discriminator =
  Tuple Discriminator
    ( case _ of
        Discriminator a → Just a
    )

-- |MediaType
type T_MediaType = {_schema :: (Maybe (ReferenceOr Schema)), _example :: (Maybe JSON), _examples :: (Maybe ((OAIMap ((ReferenceOr Example))))), _encoding :: (Maybe ((OAIMap Encoding))), _x :: (Maybe ((OAIMap JSON)))}

newtype MediaType = MediaType T_MediaType

instance eqMediaType :: Eq MediaType where
  eq (MediaType f0) (MediaType f1) = (f0._schema == f1._schema) && (f0._example == f1._example) && (f0._examples == f1._examples) && (f0._encoding == f1._encoding) && (f0._x == f1._x)


instance writeForeignMediaType :: WriteForeign MediaType where
  writeImpl (MediaType f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "schema" (writeImpl x)]) f._schema) <> (maybe [] (\x -> [Tuple "example" (writeImpl x)]) f._example) <> (maybe [] (\x -> [Tuple "examples" (writeImpl x)]) f._examples) <> (maybe [] (\x -> [Tuple "encoding" (writeImpl x)]) f._encoding) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignMediaType :: ReadForeign MediaType where
  readImpl f = do
    _schema <- (readProp "schema" f >>= readImpl) <|> (pure Nothing)
    _example <- (readProp "example" f >>= readImpl) <|> (pure Nothing)
    _examples <- (readProp "examples" f >>= readImpl) <|> (pure Nothing)
    _encoding <- (readProp "encoding" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ MediaType {_schema,_example,_examples,_encoding,_x}
_MediaType ∷
  Tuple
    ( T_MediaType → MediaType
    )
    ( MediaType →
      Maybe T_MediaType
    )
_MediaType =
  Tuple MediaType
    ( case _ of
        MediaType a → Just a
    )

data Additionals = AdditionalSchema Schema | AdditionalReference Reference | AdditionalBoolean Boolean
derive instance genericAdditionals  :: Generic Additionals  _
instance eqAdditionals :: Eq Additionals where
  eq = genericEq

readForeignAdditionalsAsObject :: Foreign -> F Additionals
readForeignAdditionalsAsObject f = do
  iref <- isRef f
  if iref then AdditionalReference <$> readImpl f else AdditionalSchema <$> readImpl f

readForeignAdditionalsAsBoolean :: Foreign -> F Additionals
readForeignAdditionalsAsBoolean f = readBoolean f >>= pure <<< AdditionalBoolean

instance readForeignAdditionals :: ReadForeign Additionals where
  readImpl f = readForeignAdditionalsAsObject f <|> readForeignAdditionalsAsBoolean f

instance writeForeignAdditionals :: WriteForeign Additionals where
  writeImpl (AdditionalSchema s) = writeImpl s
  writeImpl (AdditionalReference r) = writeImpl r
  writeImpl (AdditionalBoolean b) = writeImpl b

_AdditionalSchema ∷
  Tuple
    ( Schema → Additionals
    )
    ( Additionals →
      Maybe Schema
    )
_AdditionalSchema =
  Tuple AdditionalSchema
    ( case _ of
        AdditionalSchema a → Just a
        _ → Nothing
    )

_AdditionalReference ∷
  Tuple
    ( Reference → Additionals
    )
    ( Additionals →
      Maybe Reference
    )
_AdditionalReference =
  Tuple AdditionalReference
    ( case _ of
        AdditionalReference a → Just a
        _ → Nothing
    )

_AdditionalBoolean ∷
  Tuple
    ( Boolean → Additionals
    )
    ( Additionals →
      Maybe Boolean
    )
_AdditionalBoolean =
  Tuple AdditionalBoolean
    ( case _ of
        AdditionalBoolean a → Just a
        _ → Nothing
    )

-- |OAuthFlows
type T_OAuthFlows = {_implicit :: (Maybe ImplicitOAuthFlow), _password :: (Maybe PasswordOAuthFlow), _clientCredentials :: (Maybe ClientCredentialsFlow), _authorizationCode :: (Maybe AuthorizationCodeOAuthFlow), _x :: (Maybe ((OAIMap JSON)))}

newtype OAuthFlows = OAuthFlows T_OAuthFlows

instance eqOAuthFlows :: Eq OAuthFlows where
  eq (OAuthFlows f0) (OAuthFlows f1) = (f0._implicit == f1._implicit) && (f0._password == f1._password) && (f0._clientCredentials == f1._clientCredentials) && (f0._authorizationCode == f1._authorizationCode) && (f0._x == f1._x)


instance writeForeignOAuthFlows :: WriteForeign OAuthFlows where
  writeImpl (OAuthFlows f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "implicit" (writeImpl x)]) f._implicit) <> (maybe [] (\x -> [Tuple "password" (writeImpl x)]) f._password) <> (maybe [] (\x -> [Tuple "clientCredentials" (writeImpl x)]) f._clientCredentials) <> (maybe [] (\x -> [Tuple "authorizationCode" (writeImpl x)]) f._authorizationCode) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignOAuthFlows :: ReadForeign OAuthFlows where
  readImpl f = do
    _implicit <- (readProp "implicit" f >>= readImpl) <|> (pure Nothing)
    _password <- (readProp "password" f >>= readImpl) <|> (pure Nothing)
    _clientCredentials <- (readProp "clientCredentials" f >>= readImpl) <|> (pure Nothing)
    _authorizationCode <- (readProp "authorizationCode" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ OAuthFlows {_implicit,_password,_clientCredentials,_authorizationCode,_x}
_OAuthFlows ∷
  Tuple
    ( T_OAuthFlows → OAuthFlows
    )
    ( OAuthFlows →
      Maybe T_OAuthFlows
    )
_OAuthFlows =
  Tuple OAuthFlows
    ( case _ of
        OAuthFlows a → Just a
    )

-- |Encoding
type T_Encoding = {_contentType :: (Maybe String), _headers :: (Maybe ((OAIMap Header))), _style :: (Maybe String), _explode :: (Maybe Boolean), _allowReserved :: (Maybe Boolean), _x :: (Maybe ((OAIMap JSON)))}

newtype Encoding = Encoding T_Encoding

instance eqEncoding :: Eq Encoding where
  eq (Encoding f0) (Encoding f1) = (f0._contentType == f1._contentType) && (f0._headers == f1._headers) && (f0._style == f1._style) && (f0._explode == f1._explode) && (f0._allowReserved == f1._allowReserved) && (f0._x == f1._x)


instance writeForeignEncoding :: WriteForeign Encoding where
  writeImpl (Encoding f) =
    writeImpl $ FO.fromFoldable ((maybe [] (\x -> [Tuple "contentType" (writeImpl x)]) f._contentType) <> (maybe [] (\x -> [Tuple "headers" (writeImpl x)]) f._headers) <> (maybe [] (\x -> [Tuple "style" (writeImpl x)]) f._style) <> (maybe [] (\x -> [Tuple "explode" (writeImpl x)]) f._explode) <> (maybe [] (\x -> [Tuple "allowReserved" (writeImpl x)]) f._allowReserved) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignEncoding :: ReadForeign Encoding where
  readImpl f = do
    _contentType <- (readProp "contentType" f >>= readImpl) <|> (pure Nothing)
    _headers <- (readProp "headers" f >>= readImpl) <|> (pure Nothing)
    _style <- (readProp "style" f >>= readImpl) <|> (pure Nothing)
    _explode <- (readProp "explode" f >>= readImpl) <|> (pure Nothing)
    _allowReserved <- (readProp "allowReserved" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ Encoding {_contentType,_headers,_style,_explode,_allowReserved,_x}
_Encoding ∷
  Tuple
    ( T_Encoding → Encoding
    )
    ( Encoding →
      Maybe T_Encoding
    )
_Encoding =
  Tuple Encoding
    ( case _ of
        Encoding a → Just a
    )

-- |PasswordOAuthFlow
type T_PasswordOAuthFlow = {_tokenUrl :: String, _refreshUrl :: (Maybe String), _scopes :: (Maybe ((OAIMap String))), _x :: (Maybe ((OAIMap JSON)))}

newtype PasswordOAuthFlow = PasswordOAuthFlow T_PasswordOAuthFlow

instance eqPasswordOAuthFlow :: Eq PasswordOAuthFlow where
  eq (PasswordOAuthFlow f0) (PasswordOAuthFlow f1) = (f0._tokenUrl == f1._tokenUrl) && (f0._refreshUrl == f1._refreshUrl) && (f0._scopes == f1._scopes) && (f0._x == f1._x)


instance writeForeignPasswordOAuthFlow :: WriteForeign PasswordOAuthFlow where
  writeImpl (PasswordOAuthFlow f) =
    writeImpl $ FO.fromFoldable ([Tuple "tokenUrl" (writeImpl f._tokenUrl)] <> (maybe [] (\x -> [Tuple "refreshUrl" (writeImpl x)]) f._refreshUrl) <> (maybe [] (\x -> [Tuple "scopes" (writeImpl x)]) f._scopes) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignPasswordOAuthFlow :: ReadForeign PasswordOAuthFlow where
  readImpl f = do
    _tokenUrl <- readProp "tokenUrl" f >>= readImpl
    _refreshUrl <- (readProp "refreshUrl" f >>= readImpl) <|> (pure Nothing)
    _scopes <- (readProp "scopes" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ PasswordOAuthFlow {_tokenUrl,_refreshUrl,_scopes,_x}
_PasswordOAuthFlow ∷
  Tuple
    ( T_PasswordOAuthFlow → PasswordOAuthFlow
    )
    ( PasswordOAuthFlow →
      Maybe T_PasswordOAuthFlow
    )
_PasswordOAuthFlow =
  Tuple PasswordOAuthFlow
    ( case _ of
        PasswordOAuthFlow a → Just a
    )

-- |ImplicitOAuthFlow
type T_ImplicitOAuthFlow = {_authorizationUrl :: String, _scopes :: ((OAIMap String)), _refreshUrl :: (Maybe String), _x :: (Maybe ((OAIMap JSON)))}

newtype ImplicitOAuthFlow = ImplicitOAuthFlow T_ImplicitOAuthFlow

instance eqImplicitOAuthFlow :: Eq ImplicitOAuthFlow where
  eq (ImplicitOAuthFlow f0) (ImplicitOAuthFlow f1) = (f0._authorizationUrl == f1._authorizationUrl) && (f0._scopes == f1._scopes) && (f0._refreshUrl == f1._refreshUrl) && (f0._x == f1._x)


instance writeForeignImplicitOAuthFlow :: WriteForeign ImplicitOAuthFlow where
  writeImpl (ImplicitOAuthFlow f) =
    writeImpl $ FO.fromFoldable ([Tuple "authorizationUrl" (writeImpl f._authorizationUrl)] <> [Tuple "scopes" (writeImpl f._scopes)] <> (maybe [] (\x -> [Tuple "refreshUrl" (writeImpl x)]) f._refreshUrl) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignImplicitOAuthFlow :: ReadForeign ImplicitOAuthFlow where
  readImpl f = do
    _authorizationUrl <- readProp "authorizationUrl" f >>= readImpl
    _scopes <- readProp "scopes" f >>= readImpl
    _refreshUrl <- (readProp "refreshUrl" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ ImplicitOAuthFlow {_authorizationUrl,_scopes,_refreshUrl,_x}
_ImplicitOAuthFlow ∷
  Tuple
    ( T_ImplicitOAuthFlow → ImplicitOAuthFlow
    )
    ( ImplicitOAuthFlow →
      Maybe T_ImplicitOAuthFlow
    )
_ImplicitOAuthFlow =
  Tuple ImplicitOAuthFlow
    ( case _ of
        ImplicitOAuthFlow a → Just a
    )

-- |AuthorizationCodeOAuthFlow
type T_AuthorizationCodeOAuthFlow = {_tokenUrl :: String, _authorizationUrl :: String, _refreshUrl :: (Maybe String), _scopes :: (Maybe ((OAIMap String))), _x :: (Maybe ((OAIMap JSON)))}

newtype AuthorizationCodeOAuthFlow = AuthorizationCodeOAuthFlow T_AuthorizationCodeOAuthFlow

instance eqAuthorizationCodeOAuthFlow :: Eq AuthorizationCodeOAuthFlow where
  eq (AuthorizationCodeOAuthFlow f0) (AuthorizationCodeOAuthFlow f1) = (f0._tokenUrl == f1._tokenUrl) && (f0._authorizationUrl == f1._authorizationUrl) && (f0._refreshUrl == f1._refreshUrl) && (f0._scopes == f1._scopes) && (f0._x == f1._x)


instance writeForeignAuthorizationCodeOAuthFlow :: WriteForeign AuthorizationCodeOAuthFlow where
  writeImpl (AuthorizationCodeOAuthFlow f) =
    writeImpl $ FO.fromFoldable ([Tuple "tokenUrl" (writeImpl f._tokenUrl)] <> [Tuple "authorizationUrl" (writeImpl f._authorizationUrl)] <> (maybe [] (\x -> [Tuple "refreshUrl" (writeImpl x)]) f._refreshUrl) <> (maybe [] (\x -> [Tuple "scopes" (writeImpl x)]) f._scopes) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignAuthorizationCodeOAuthFlow :: ReadForeign AuthorizationCodeOAuthFlow where
  readImpl f = do
    _tokenUrl <- readProp "tokenUrl" f >>= readImpl
    _authorizationUrl <- readProp "authorizationUrl" f >>= readImpl
    _refreshUrl <- (readProp "refreshUrl" f >>= readImpl) <|> (pure Nothing)
    _scopes <- (readProp "scopes" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ AuthorizationCodeOAuthFlow {_tokenUrl,_authorizationUrl,_refreshUrl,_scopes,_x}
_AuthorizationCodeOAuthFlow ∷
  Tuple
    ( T_AuthorizationCodeOAuthFlow → AuthorizationCodeOAuthFlow
    )
    ( AuthorizationCodeOAuthFlow →
      Maybe T_AuthorizationCodeOAuthFlow
    )
_AuthorizationCodeOAuthFlow =
  Tuple AuthorizationCodeOAuthFlow
    ( case _ of
        AuthorizationCodeOAuthFlow a → Just a
    )

-- |ClientCredentialsFlow
type T_ClientCredentialsFlow = {_tokenUrl :: String, _refreshUrl :: (Maybe String), _scopes :: (Maybe ((OAIMap String))), _x :: (Maybe ((OAIMap JSON)))}

newtype ClientCredentialsFlow = ClientCredentialsFlow T_ClientCredentialsFlow

instance eqClientCredentialsFlow :: Eq ClientCredentialsFlow where
  eq (ClientCredentialsFlow f0) (ClientCredentialsFlow f1) = (f0._tokenUrl == f1._tokenUrl) && (f0._refreshUrl == f1._refreshUrl) && (f0._scopes == f1._scopes) && (f0._x == f1._x)


instance writeForeignClientCredentialsFlow :: WriteForeign ClientCredentialsFlow where
  writeImpl (ClientCredentialsFlow f) =
    writeImpl $ FO.fromFoldable ([Tuple "tokenUrl" (writeImpl f._tokenUrl)] <> (maybe [] (\x -> [Tuple "refreshUrl" (writeImpl x)]) f._refreshUrl) <> (maybe [] (\x -> [Tuple "scopes" (writeImpl x)]) f._scopes) <> (maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x))

instance readForeignClientCredentialsFlow :: ReadForeign ClientCredentialsFlow where
  readImpl f = do
    _tokenUrl <- readProp "tokenUrl" f >>= readImpl
    _refreshUrl <- (readProp "refreshUrl" f >>= readImpl) <|> (pure Nothing)
    _scopes <- (readProp "scopes" f >>= readImpl) <|> (pure Nothing)
    _x <- xify f
    pure $ ClientCredentialsFlow {_tokenUrl,_refreshUrl,_scopes,_x}
_ClientCredentialsFlow ∷
  Tuple
    ( T_ClientCredentialsFlow → ClientCredentialsFlow
    )
    ( ClientCredentialsFlow →
      Maybe T_ClientCredentialsFlow
    )
_ClientCredentialsFlow =
  Tuple ClientCredentialsFlow
    ( case _ of
        ClientCredentialsFlow a → Just a
    )

