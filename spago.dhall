{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "URLNote"
, dependencies =
  [ "aff"
  , "arrays"
  , "bigints"
  , "effect"
  , "enums"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
