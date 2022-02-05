{ name = "lingo"
, dependencies =
  [ "effect"
  , "console"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
