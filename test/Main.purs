module Main where

import Prelude

import Data.OpenAPI.V300(OpenAPIObject(..))
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff as Effect
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (E, readJSON)
import Test.Spec (before, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


readOpenAPI :: String -> Effect.Aff (E OpenAPIObject)
readOpenAPI t = do
  rtf <- liftEffect $ readTextFile UTF8 t
  pure $ readJSON rtf

-- This is the main function that does the PBT and shows the results
-- It should show { failures: Nil, successes: 100, total: 100 }
-- To run more than 100 tests, change the number 100 below
main ∷ Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "stripe" do
    before (readOpenAPI "oai/stripe.json") $ do
      it "has version 3.0.0" $ \oai -> do
        --getOpenAPIObjectOpenapi oai `shouldBe` "3.0.0"
        either (\e -> fail (show e)) (\(OpenAPIObject { _openapi }) -> _openapi `shouldEqual` "3.0.0") oai
  describe "slack" do
    before (readOpenAPI "oai/slack.json") $ do
      it "has version 3.0.0" $ \oai -> do
        --getOpenAPIObjectOpenapi oai `shouldBe` "3.0.0"
        either (\e -> fail (show e)) (\(OpenAPIObject { _openapi }) -> _openapi `shouldEqual` "3.0.0") oai
