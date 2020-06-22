{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-react"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "concur-core"
  , "console"
  , "foldable-traversable"
  , "free"
  , "nonempty"
  , "profunctor-lenses"
  , "react"
  , "react-basic"
  , "react-dom"
  , "tailrec"
  , "web-dom"
  , "web-html"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-react"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
