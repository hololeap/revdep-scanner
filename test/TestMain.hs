
module Main (main) where

import Test.Tasty

import RevdepScanner.Test.UnitTests

main :: IO ()
main = defaultMain $ testGroup "revdep-scanner tests"
    [ simpleTests
    ]
