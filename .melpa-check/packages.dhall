let Schema = ./schema.dhall

let Package = Schema.Package

let TestDriver = Schema.TestDriver

in  [ Package::{
      , pname = "use-package-tags"
      , version = "0.1"
      , emacsVersion = "25.1"
      , files = [ "use-package-tags.el" ]
      , dependencies = [ "dash" ] : List Text
      , testDependencies = [ "use-package" ]
      , testDrivers = [ TestDriver.buttercup ]
      , buttercupTests = [ "*-test.el" ]
      , recipe =
          ''
          (use-package-tags :fetcher github :repo "akirak/use-package-tags")
          ''
      }
    ]
