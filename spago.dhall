{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-signals"
, dependencies =
  [ "control"
  , "concur-core"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lazy"
  , "maybe"
  , "prelude"
  , "tuples"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-core"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
