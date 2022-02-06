{ name = "lingo"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
