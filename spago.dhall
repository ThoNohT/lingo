{ name = "lingo"
, dependencies =
  [ "affjax"
  , "aff"
  , "either"
  , "console"
  , "effect"
  , "halogen"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
