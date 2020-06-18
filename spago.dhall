{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "openapi"
, license  = "Apache-2.0"
, repository = "https://github.com/meeshkan/purescript-openapi"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "ordered-collections"
  , "psci-support"
  , "simple-json"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
