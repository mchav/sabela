module Main (main) where

import Test.Tasty

import Test.Parse (parseTests)
import Test.Transform (transformTests)
import Test.Markdown (markdownTests)
import Test.Integration (integrationTests)

main :: IO ()
main = defaultMain $ testGroup "HScript"
  [ parseTests
  , transformTests
  , markdownTests
  , integrationTests
  ]
