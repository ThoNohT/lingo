{ name = "lingo"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
