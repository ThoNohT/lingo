{ name = "lingo"
, dependencies =
  [ "aff", "console", "effect", "halogen", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
