{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = 
    [ "affjax"
    , "argonaut" 
    , "console"
    , "effect"
    , "halogen"
    , "psci-support"
    , "uri" 
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
