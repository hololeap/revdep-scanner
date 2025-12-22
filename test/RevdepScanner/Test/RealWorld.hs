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
import qualified Data.Map.Monoidal.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

import Data.Parsable
import Distribution.Gentoo.Utils.Pquery
import Distribution.Portage.Types

import RevdepScanner.Types.ConstraintMap
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
        Just () @=? do
            a <- HM.lookup (runParsable' rndm) cm
            b <- M.lookup (runParsable' pkg) $ NEM.toMap $ Just a
            c <- M.lookup DEPEND $ NEM.toMap $ Just b
            (d,_) <- getCtx c
            e <- M.lookup Nothing $ NEM.toMap $ Just d
            M.lookup (runParsable' rndm)
                $ NEM.toMap $ Just $ getDepMap e
    ]

runParsable' :: Parsable a PureMode String => String -> a
runParsable' s =
    let (Right x) = runParsable (encodeString s)
    in x

getCtx :: CTX.ContextMap 'Nothing -> Maybe (NEM.NEMMap (Maybe DepContext) (DepMap 'Nothing), NEM.NEMMap OrContext (OrGroupMap 'Nothing))
getCtx = \case
    CTX.MixedCtxMap nm om -> Just (nm, om)
    _ -> Nothing
