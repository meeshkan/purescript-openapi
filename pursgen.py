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
            data = 'type T_%s = {' % (nm, )
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
            toexpt = {'%s(..)' % nm, 'T_%s' % nm, '_%s' % nm, *toexpt}
            prism = '''_%s ∷
  Tuple
    ( T_%s → %s
    )
    ( %s →
      Maybe T_%s
    )
_%s =
  Tuple %s
    ( case _ of
        %s a → Just a
    )
''' % (nm, nm, nm, nm, nm, nm, nm, nm)
            return comment + '\n' + data + '\n\n' + ("newtype %s = %s T_%s\n\n" % (nm, nm, nm)) + makeEq + '\n\n' + toJSONs + '\n' + fromJSONs + '\n' + prism, todo, {ipt, *done}, toexpt
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
'''
                        return out, todo, {ipt, *done}, {'Additionals(..)', '_AdditionalBoolean', '_AdditionalReference', '_AdditionalSchema',  *toexpt}
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
'''
                        return out, todo, {ipt, *done}, {'Items(..)', '_SingleItemReference', '_SingleItem', '_ItemsAsTuple', *toexpt}
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
                    out = '''data SecuritySchema = APIKeySS APIKeySecurityScheme | 
      HTTPSS HTTPSecurityScheme |
      OAuth2SS OAuth2SecurityScheme | 
      OpenIdConnectSS OpenIdConnectSecurityScheme | 
      StringSS String | 
      ReferenceSS Reference

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
'''
                    return out, {APIKeySecurityScheme, HTTPSecurityScheme, OAuth2SecurityScheme, OpenIdConnectSecurityScheme, str, Reference, *todo}, {ipt, *done}, {'SecuritySchema(..)', 
                          '_APIKeySS',
      '_HTTPSS' ,
      '_OAuth2SS',
      '_OpenIdConnectSS' , 
      '_StringSS' , 
      '_ReferenceSS' ,
 *toexpt}
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

_OAIMap ::
  forall b.
  Tuple
    ( Map.Map String b -> OAIMap b
    )
    ( OAIMap b ->
      Maybe (Map.Map String b)
    )
_OAIMap =
  Tuple OAIMap
    ( case _ of
        OAIMap a -> Just a
    )

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
'''
    out += 'hack :: ∀ a b c. (a -> c) -> (a -> b -> c)\nhack o = (\\x -> (\\y -> o x))\n\n'
    out += '''xify :: Foreign -> F (Maybe (OAIMap JSON))
xify f = do
  (OAIMap asMap) <- (readImpl f) :: (F (OAIMap JSON))
  pure  $ Just (OAIMap (Map.filterKeys (startsWith "x-") asMap))
'''
    out += 'isRef :: Foreign -> F Boolean\n'
    out += 'isRef f = keys f >>= pure <<< (/=) Nothing <<< findIndex ((==) "$ref")\n\n'
    out += '''data ReferenceOr a = Ref Reference | RealDeal a
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
'''

    toexpt = []
    while len(todo) > 0:
        todo = {x for x in todo if x not in done}
        for item in {*todo}:
            o, todo, done, toexpt = to_purescript(item, todo, done, toexpt)
            out += o+'\n'
    out = out.replace('FOOBAR', ',\n '.join({'ReferenceOr(..)', '_Ref', '_RealDeal', 'BooleanInt', 'OAIMap(..)', '_OAIMap', '_ABoolean', '_AnInt', 'JSON(..)', *toexpt}))
    with open('./src/Data/OpenAPI/V300.purs', 'w', encoding='utf-8') as hask:
        hask.write(out)