{ name = "lingo"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "tuples"
  , "effect"
  , "either"
  , "halogen"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "foldable-traversable"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
