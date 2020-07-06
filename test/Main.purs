module Test.Main where

import Prelude
import Data.Array (mapWithIndex)
import Data.Either (either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Map (toUnfoldable)
import Data.OpenAPI.V300 (JSON(..), OAIMap(..), OpenAPIObject)
import Data.Set (difference, empty, singleton, Set, union, size)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff as Effect
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (readJSON, writeJSON)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

readOpenAPI :: String -> Effect.Aff OpenAPIObject
readOpenAPI t = do
  rtf <- liftEffect $ readTextFile UTF8 t
  either (liftEffect <<< throw <<< const "Could not parse openapi") pure $ readJSON rtf

readAsJSON :: String -> Effect.Aff JSON
readAsJSON t = do
  rtf <- liftEffect $ readTextFile UTF8 t
  either (liftEffect <<< throw <<< const "Could not parse json") pure $ readJSON rtf

readAsJSON' :: String -> Effect.Aff JSON
readAsJSON' t = do
  either (liftEffect <<< throw <<< const "Could not parse json") pure $ readJSON t

data SimpleJSON
  = SJNumber Number
  | SJBoolean Boolean
  | SJString String
  | SJNull

derive instance genericSimpleJSON :: Generic SimpleJSON _

instance showSimpleJSON :: Show SimpleJSON where
  show = genericShow

derive instance eqSimpleJSON :: Eq SimpleJSON

derive instance ordSimpleJSON :: Ord SimpleJSON

flatten :: JSON -> Set (Tuple (List String) SimpleJSON)
flatten = flatten' Nil
  where
  flatten' :: List String -> JSON -> Set (Tuple (List String) SimpleJSON)
  flatten' l JNull = singleton (Tuple l SJNull)

  flatten' l (JBoolean b) = singleton (Tuple l (SJBoolean b))

  flatten' l (JNumber n) = singleton (Tuple l (SJNumber n))

  flatten' l (JString s) = singleton (Tuple l (SJString s))

  flatten' l (JArray a) = foldl union empty $ mapWithIndex (\i v -> flatten' (l <> (show i : Nil)) v) a

  flatten' l (JObject (OAIMap o)) = foldl union empty $ map (\(Tuple k v) -> flatten' (l <> (k : Nil)) v) (toUnfoldable o :: List (Tuple String JSON))

openAPITest :: String -> Effect.Aff Unit
openAPITest fn = do
  asOpenAPI <- readOpenAPI fn
  asJSON <- readAsJSON fn
  asReJson <- readAsJSON' (writeJSON asOpenAPI)
  let
    flatJSON = flatten asJSON
  let
    flatReJson = flatten asReJson
  -- sanity check to make sure that this is biiigggg
  (size flatJSON > 100) `shouldEqual` true
  size flatJSON `shouldEqual` size flatReJson
  union (difference flatJSON flatReJson) (difference flatReJson flatJSON) `shouldEqual` empty

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "stripe" do
          it "should parse, unparse and reparse correctly" $ openAPITest "oai/stripe.json"
        describe "stripe" do
          it "should parse, unparse and reparse correctly" $ openAPITest "oai/slack.json"
