{-# Options_GHC -Wno-incomplete-uni-patterns #-}

{-# Language DataKinds #-}
{-# Language LambdaCase #-}
{-# Language OverloadedLists #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module RevdepScanner.Test.RealWorld
    ( realWorldTests
    ) where

import qualified Data.HashMap.Monoidal as HM
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Monoidal.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

import Data.Parsable
import Distribution.Gentoo.Utils.Pquery
import Distribution.Portage.Types

import RevdepScanner.Logic
import RevdepScanner.Types.DepMap
import qualified RevdepScanner.Types.ContextMap as CTX
import qualified RevdepScanner.Types.NEMMap as NEM
import RevdepScanner.Types
import RevdepScanner.Util

import qualified Paths_revdep_scanner as P

realWorldTests :: TestTree
realWorldTests = testGroup "real-world tests"
    [ let pkg = "dev-haskell/text-icu-0.8.0.4"
          rndm = "dev-haskell/random"
      in  testCase pkg $ do
        fn <- P.getDataFileName $ "test/data/" ++ pkg ++ ".show"
        s <- readFile fn
        let pd = (read s) :: PkgDeps
            cm = buildCMap pd
            ctx = UseCtx
                    (NEL.fromList
                        [ Right $ runParsable' ">=dev-haskell/hunit-1.2"
                        , Right $ runParsable' ">=dev-haskell/quickcheck-2.4"
                        , Right $ runParsable' "dev-haskell/random"
                        , Right $ runParsable' ">=dev-haskell/test-framework-0.4"
                        , Right $ runParsable' ">=dev-haskell/test-framework-hunit-0.2"
                        , Right $ runParsable' ">=dev-haskell/test-framework-quickcheck2-0.2"
                        , Right $ runParsable' "dev-haskell/text"
                        ]
                    ) "test"
            dm :: DepMap 'Nothing
            dm = DepMap $ NEM.singleton (runParsable' "dev-haskell/random") ()
        Just dm @=? do
            a <- HM.lookup (runParsable' rndm) cm
            b <- M.lookup (runParsable' pkg) $ NEM.toMap $ Just a
            c <- M.lookup DEPEND $ NEM.toMap $ Just b
            d <- getCtx c
            M.lookup (Just ctx) $ NEM.toMap $ Just d
    ]

runParsable' :: Parsable a PureMode String => String -> a
runParsable' s =
    let (Right x) = runParsable (encodeString s)
    in x

getCtx :: CTX.ContextMap 'Nothing -> Maybe (NEM.NEMMap (Maybe DepContext) (DepMap 'Nothing))
getCtx = \case
    CTX.NormalCtxMap nm -> Just nm
    _ -> Nothing
