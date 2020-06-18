# script to generate purescript types from openapi typed 2

from openapi_typed_2 import OpenAPIObject, Reference, APIKeySecurityScheme, APIKeySecurityScheme, HTTPSecurityScheme, OAuth2SecurityScheme, OpenIdConnectSecurityScheme, Response
import openapi_typed_2 as openapi
from collections.abc import Mapping, Sequence
from typing import Union, Any, ForwardRef
import typing

def px(a):
    return '(%s)' % a if ' ' in a else a


def to_purescript(ipt, todo, done, toexpt, top=True):
    if hasattr(ipt, '__dataclass_fields__'):
        if top:
            nm = ipt.__name__
            comment = '-- |%s' % nm
            data = 'data %s = %s {' % (nm, nm)
            accessors = []
            dashize = lambda cstr : '_'+cstr if cstr != '$ref' else '_ref'
            for k, v in ipt.__dataclass_fields__.items():
                srep, td, dn, txp = to_purescript(v.type, todo, done, toexpt, False)
                todo = {*td, *todo}
                done = {*dn, *done}
                toexpt = {*txp, *toexpt}
                cstr = (k if k[0] != '_' else '$ref' if k == '_ref' else k[1:])
                data += '%s :: %s, ' % (dashize(cstr), px(srep))
                accessors = [*accessors, (cstr, srep)]
            data = data[:-2] + '}'
            dashes = lambda pos, l : ' '.join(['r' if x == pos else '_' for x in range(l)])
            udashes = lambda pos, l : ' '.join(['_' if x == pos else '_%d' % x for x in range(l)])
            ndashes = lambda pos, l : ' '.join(['_new_' if x == pos else '_%d' % x for x in range(l)])
            dashproto = [(dashize(cstr), _) for cstr, _ in accessors]
            dashvars = [x for x, _ in dashproto]
            fromJSONs = 'instance readForeign%s :: ReadForeign %s where\n  readImpl f = do\n%s\n    pure $ %s %s' % (nm, nm, '\n'.join([('    %s <- %s' % (dashize(cstr), 'readProp "%s" f >>= readImpl' % cstr if dstr[:5] != 'Maybe' else '(readProp "%s" f >>= readImpl) <|> (pure Nothing)' % cstr)) if cstr != 'x' else '    _x <- xify f' for i, (cstr, dstr) in enumerate(accessors)]), nm, '{' + ','.join([dashize(cstr) for (cstr, _) in accessors]) + '}' )
            toJSONs = 'instance writeForeign%s :: WriteForeign %s where\n  writeImpl (%s f) =\n    writeImpl $ FO.fromFoldable (%s)\n' % (nm, nm, nm, ' <> '.join([('[Tuple "%s" (writeImpl f.%s)]' % (cstr, '_'+cstr if cstr != '$ref' else '_ref')) if dstr[:5] != 'Maybe' else ('(maybe [] (\\x -> [Tuple "%s" (writeImpl x)]) f.%s)' % (cstr, '_'+cstr if cstr != '$ref' else '_ref')) if cstr != 'x' else '(maybe [] (\(OAIMap x) -> map (\(Tuple a b) -> (Tuple a $ writeImpl b)) (Map.toUnfoldable x)) f._x)' for cstr, dstr in accessors]))
            #makeShow = 'instance show%s :: Show %s where\n  show (%s f) = show ("%s" <> "(" <> intercalate ", " (filter (not <<< null) [%s]) <> ")")' % (nm, nm, nm, nm, ', '.join(['"%s = " <> show f.%s' % (cstr[1:], cstr) if dstr[:5] != 'Maybe'  else 'maybe "" (\\x -> "%s = Just " <> show x) f.%s' % (cstr[1:], cstr) for cstr, dstr in dashproto]))
            makeEq = 'instance eq%s :: Eq %s where\n  eq (%s f0) (%s f1) = %s\n' % (nm, nm, nm, nm, ' && '.join(['(f0.%s == f1.%s)' % (v,v) for (v, x) in dashproto ]))
            toexpt = {'%s(..)' % nm, *toexpt}
            return comment + '\n' + data + '\n\n' + makeEq + '\n\n' + toJSONs + '\n' + fromJSONs + '\n', todo, {ipt, *done}, toexpt
        else:
            return (ipt.__name__, {ipt, *todo}, done, toexpt)
    elif ipt == str:
        return 'String', todo, {ipt, *done}, toexpt
    elif ipt == int:
        return 'Int', todo, {ipt, *done}, toexpt
    elif ipt == Any:
        return 'JSON', todo, {ipt, *done}, toexpt
    elif ipt == bool:
        return 'Boolean', todo, {ipt, *done}, toexpt
    elif ipt == float:
        return 'Number', todo, {ipt, *done}, toexpt
    elif hasattr(ipt, '__origin__'):
        if ipt.__origin__ == Mapping:
            srep, td, dn, txp = to_purescript(ipt.__args__[1], todo, done, toexpt, False)
            return '(OAIMap %s)' % px(srep), {*td, *todo}, {*dn, *done}, {*txp, *toexpt}
        if ipt.__origin__ == Union:
            if len(ipt.__args__) == 4:
                # hack
                if bool in ipt.__args__:
                    if top:
                        out = '''data Additionals = AdditionalSchema Schema | AdditionalReference Reference | AdditionalBoolean Boolean
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
'''
                        out += '''
eitherAdditionalSchema :: Additionals -> Either Additionals Schema
eitherAdditionalSchema (AdditionalSchema r) = Right r
eitherAdditionalSchema l = Left l

eitherAdditionalReference :: Additionals -> Either Additionals Reference
eitherAdditionalReference (AdditionalReference r) = Right r
eitherAdditionalReference l = Left l

eitherAdditionalBoolean :: Additionals -> Either Additionals Boolean
eitherAdditionalBoolean (AdditionalBoolean r) = Right r
eitherAdditionalBoolean l = Left l
'''
                        return out, todo, {ipt, *done}, {'Additionals(..)',  *toexpt}
                    else:
                        return 'Maybe Additionals', {ipt, *todo}, done, toexpt
                else:
                    if top:
                        out = '''
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
'''
                        out += '''
eitherItemsAsTuple :: Items -> Either Items (Array (ReferenceOr Schema))
eitherItemsAsTuple (ItemsAsTuple r) = Right r
eitherItemsAsTuple l = Left l

eitherSingleItem :: Items -> Either Items Schema
eitherSingleItem (SingleItem r) = Right r
eitherSingleItem l = Left l

eitherSingleItemReference :: Items -> Either Items Reference
eitherSingleItemReference (SingleItemReference r) = Right r
eitherSingleItemReference l = Left l
'''
                        return out, todo, {ipt, *done}, {'Items(..)',  *toexpt}
                    else:
                        return 'Maybe Items', {ipt, *todo}, done, toexpt
            if len(ipt.__args__) == 2 and (type(None) in ipt.__args__):
                notnone = [g for g in ipt.__args__ if g != type(None)][0]
                srep, td, dn, txp = to_purescript(notnone, todo, done, toexpt, False)
                return 'Maybe %s' % px(srep), {*td, *todo}, {*dn, *done}, {*txp, *toexpt}
            if len(ipt.__args__) == 2 and (Reference in ipt.__args__):
                ipt.__args__ = [x if x.__class__.__name__ != 'ForwardRef' else getattr(openapi, x.__forward_arg__) for x in ipt.__args__]
                notref = [g for g in ipt.__args__ if g != Reference][0]
                srep, td, dn, txp = to_purescript(notref, todo, done, toexpt, False)
                return '(ReferenceOr %s)' % px(srep), {*td, *todo}, {*dn, *done}, toexpt
            if len(ipt.__args__) == 3 and (Reference in ipt.__args__) and (type(None) in ipt.__args__):
                notthing = [g for g in ipt.__args__ if (g != Reference) and (g != type(None))][0]
                srep, td, dn, txp = to_purescript(notthing, todo, done, toexpt, False)
                return 'Maybe (ReferenceOr %s)' % px(srep), {*td, *todo}, {*dn, *done}, {*txp, *toexpt}
            if ipt == Union[bool, int, type(None)]:
                return 'Maybe BooleanInt', todo, done, toexpt
            if APIKeySecurityScheme in ipt.__args__:
                if top:
                    out = '''data SecuritySchema = APIKeySS APIKeySecurityScheme | HTTPSS HTTPSecurityScheme | OAuth2SS OAuth2SecurityScheme | OpenIdConnectSS OpenIdConnectSecurityScheme | StringSS String | ReferenceSS Reference
derive instance genericSecuritySchema  :: Generic SecuritySchema  _
instance eqSecuritySchema :: Eq SecuritySchema where
  eq = genericEq'''
                    out += '''
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
'''
                    out += '''
eitherAPIKeySS :: SecuritySchema -> Either SecuritySchema APIKeySecurityScheme
eitherAPIKeySS (APIKeySS r) = Right r
eitherAPIKeySS l = Left l

eitherHTTPSS :: SecuritySchema -> Either SecuritySchema HTTPSecurityScheme
eitherHTTPSS (HTTPSS r) = Right r
eitherHTTPSS l = Left l

eitherOAuth2SS :: SecuritySchema -> Either SecuritySchema OAuth2SecurityScheme
eitherOAuth2SS (OAuth2SS r) = Right r
eitherOAuth2SS l = Left l

eitherOpenIdConnectSS :: SecuritySchema -> Either SecuritySchema OpenIdConnectSecurityScheme
eitherOpenIdConnectSS (OpenIdConnectSS r) = Right r
eitherOpenIdConnectSS l = Left l
'''
                    return out, {APIKeySecurityScheme, HTTPSecurityScheme, OAuth2SecurityScheme, OpenIdConnectSecurityScheme, str, Reference, *todo}, {ipt, *done}, {'SecuritySchema(..)', *toexpt}
                else:
                    return 'SecuritySchema', {ipt, *todo}, done, toexpt
        if ipt.__origin__ == Sequence:
            srep, td, dn, txp = to_purescript(ipt.__args__[0], todo, done, toexpt, False)
            return '(Array %s)' % srep, {*td, *todo}, {*dn, *done}, {*txp, *toexpt}
    elif ipt == 'Responses':
        return to_purescript(typing.Mapping[str, Union[Response, Reference]], todo, done, toexpt, False)
    elif ipt.__class__.__name__ == 'ForwardRef':
        return to_purescript(getattr(openapi, ipt.__forward_arg__), todo, done, toexpt, False)
    raise ValueError('cannot figure out %s' % str(ipt))
        

if __name__ == '__main__':
    todo = {OpenAPIObject}
    done = {}
    out = 'module Data.OpenAPI.V300 (FOOBAR) where\n\n'
    out += '''import Prelude
import Control.Alt ((<|>))
import Data.Array (findIndex)
import Data.Either (Either(..))
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


oaiMapToObject :: forall a. (WriteForeign a) => OAIMap a -> FO.Object a
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
'''
    out += 'hack :: forall a b c. (a -> c) -> (a -> b -> c)\nhack o = (\\x -> (\\y -> o x))\n\n'
    out += '''xify :: Foreign -> F (Maybe (OAIMap JSON))
xify f = do
  (OAIMap asMap) <- (readImpl f) :: (F (OAIMap JSON))
  pure  $ Just (OAIMap (Map.filterKeys (startsWith "x-") asMap))
'''
    out += 'isRef :: Foreign -> F Boolean\n'
    out += 'isRef f = keys f >>= pure <<< (/=) Nothing <<< findIndex ((==) "$ref")\n\n'
    out += '''data ReferenceOr a = Ref Reference | RealDeal a
derive instance eqReferenceOr :: (Eq a) => Eq (ReferenceOr a)
'''
    out += '''instance readForeignReferenceOr :: (ReadForeign a) => ReadForeign (ReferenceOr a) where
  readImpl f = do
    iref <- isRef f
    if iref then Ref <$> readImpl f else RealDeal <$> readImpl f

instance writeForeignReferenceOr :: (WriteForeign a) => WriteForeign (ReferenceOr a) where
  writeImpl (Ref t) = writeImpl t
  writeImpl (RealDeal t) = writeImpl t
'''
    out += '''data BooleanInt = ABoolean Boolean | AnInt Int
derive instance genericBooleanInt  :: Generic BooleanInt  _
instance eqBooleanInt :: Eq BooleanInt where
  eq = genericEq
'''
    out += '''instance readForeignBooleanInt ::  ReadForeign BooleanInt where
  readImpl f = (readBoolean f >>= pure <<< ABoolean) <|> (readInt f >>= pure <<< AnInt)

instance writeForeignBooleanInt :: WriteForeign BooleanInt where
  writeImpl (ABoolean b) = writeImpl b
  writeImpl (AnInt i) = writeImpl i
'''
    out += '''eitherRef :: forall a. ReferenceOr a -> Either (ReferenceOr a) Reference
eitherRef (Ref r) = Right r
eitherRef l = Left l

eitherRealDeal :: forall a. ReferenceOr a -> Either (ReferenceOr a) a
eitherRealDeal (RealDeal r) = Right r
eitherRealDeal l = Left l

'''
    toexpt = []
    while len(todo) > 0:
        todo = {x for x in todo if x not in done}
        for item in {*todo}:
            o, todo, done, toexpt = to_purescript(item, todo, done, toexpt)
            out += o+'\n'
    out = out.replace('FOOBAR', ',\n '.join({'ReferenceOr', 'BooleanInt', 'OAIMap', 'JSON', *toexpt}))
    with open('./src/Data/OpenAPI/V300.purs', 'w') as hask:
        hask.write(out)