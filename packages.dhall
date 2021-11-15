let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210919/packages.dhall sha256:03516fdd4f6d1bd6c9eb5e63cf3af3037bc079459117ab93eb85b6eb46e258a7

let overrides = {=}

let additions =
      { typelevel-peano =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/csicar/purescript-typelevel-peano.git"
        , version = "v1.0.1"
        }
      , jit =
        { dependencies = [ "maybe" ]
        , repo = "https://github.com/mikesol/purescript-jit.git"
        , version = "v0.0.0"
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.4.2"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.1.0"
        }
      , wags =
        { dependencies =
          [ "aff-promise"
          , "arraybuffer-types"
          , "behaviors"
          , "console"
          , "convertable-options"
          , "debug"
          , "effect"
          , "event"
          , "free"
          , "heterogeneous"
          , "indexed-monad"
          , "maybe"
          , "ordered-collections"
          , "profunctor-lenses"
          , "psci-support"
          , "record"
          , "simple-json"
          , "sized-vectors"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/mikesol/purescript-wags.git"
        , version = "v0.5.10"
        }
      , free =
        { dependencies =
          [ "catenable-lists"
          , "control"
          , "distributive"
          , "either"
          , "exists"
          , "foldable-traversable"
          , "invariant"
          , "lazy"
          , "maybe"
          , "prelude"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-free.git"
        , version = "master"
        }
      , wags-lib =
        { dependencies =
          [ "wags"
          , "run"
          , "halogen"
          , "halogen-css"
          , "homogeneous"
          , "string-parsers"
          , "strings"
          ]
        , repo = "https://github.com/mikesol/purescript-wags-lib.git"
        , version = "v0.0.51"
        }
      , painting =
        { dependencies =
          [ "canvas"
          , "colors"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "foreign-object"
          , "psci-support"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-painting.git"
        , version = "v0.0.0"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      }

in  upstream // overrides // additions
