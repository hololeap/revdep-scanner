{-# Language DataKinds #-}
{-# Language OverloadedLists #-}
{-# Language ScopedTypeVariables #-}

module RevdepScanner.Test.UnitTests
    ( simpleTests
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.Strict as M
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit
import Validation

import Distribution.Gentoo.Utils.Pquery
import Distribution.Portage.Types
import Data.Parsable

import RevdepScanner.CmdLine
import RevdepScanner.Logic
import RevdepScanner.Types
import RevdepScanner.Types.ConstraintMap
import           RevdepScanner.Types.ContextMap (ContextMap(..), evalContextMap)
import RevdepScanner.Types.DepMap
import RevdepScanner.Types.ResultMap
import RevdepScanner.Util

simpleTests :: TestTree
simpleTests = testGroup "simple tests"
    [ parseTests
    , example1
    , example2
    , example3
    , example_Or
    ]


-- PARSE TESTS --

parseTests :: TestTree
parseTests = testGroup "parse tests"
    [ testCase "CmdlinePackage" $
        inputString "app-misc/foo" @?=
            CmdlinePackage (Package (Category "app-misc") (PkgName "foo"))
    , testCase "CmdlinePkgWithVer" $
        inputString "app-misc/foo-0.1" @?=
            CmdlinePkgWithVer
                ( PkgWithVer
                    (Package (Category "app-misc") (PkgName "foo"))
                    (Version
                        (VersionNum (NE.singleton '0' NE.:| [NE.singleton '1']))
                        Nothing
                        []
                        Nothing
                    )
                )
    , testCase "VPkgEq" $
        inputString "=app-misc/foo-0.1" @?=
            CmdlinePkgWithVer
                ( PkgWithVer
                    (Package (Category "app-misc") (PkgName "foo"))
                    (Version
                        (VersionNum (NE.singleton '0' NE.:| [NE.singleton '1']))
                        Nothing
                        []
                        Nothing
                    )
                )
    , let i = ">=app-misc/foo-0.1"
          e :: Maybe CmdlinePkg
          e = Just $ CmdlinePkgWithVer $
                PkgWithVer
                    (Package (Category "app-misc") (PkgName "foo"))
                    (Version
                        (VersionNum (NE.singleton '0' NE.:| [NE.singleton '1']))
                        Nothing
                        []
                        Nothing
                    )
          r = successToMaybe (parsePkg i)
      in testCase "VPkgGE" $
        when (e == r) (assertFailure (i ++ " should NOT equal " ++ show e))
    ]

-- EXAMPLE 1 --

example1 :: TestTree
example1 = testGroup "example 1"
    [ testCase "parsing" $ buildCMap pd1 @?= cm1
    , testGroup "isSpecRelevant"
        [ testsMatching
            [ isSpecRelevantTest input1 depSpec1 True
            , isSpecRelevantTest verInput1 depSpec1 True
            ]
        , testsNonMatching
            [ isSpecRelevantTest input1 depSpec1 False
            , isSpecRelevantTest verInput1 depSpec1 False
            ]
        ]
    , testGroup "evalDepMap"
        [ let ex = Just $ DepMap $ NEM.singleton depSpec1 (All True)
          in  testsMatching
                [ evalDepMapTest input1 udm1 ex
                , evalDepMapTest verInput1 udm1 ex
                ]
        , testsNonMatching
            [ evalDepMapTest input1 udm1 Nothing
            , evalDepMapTest verInput1 udm1 Nothing
            ]
        ]
    , testGroup "evalContextMap"
        [ testsMatching
            [ evalContextMapTest input1 ucm1 (Just rcm1)
            , evalContextMapTest verInput1 ucm1 (Just rcm1)
            ]
        , testsNonMatching
            [ evalContextMapTest input1 ucm1 Nothing
            , evalContextMapTest verInput1 ucm1 Nothing
            ]
        ]
    , testGroup "lookupResults"
        [ testsMatching
            [ lookupResultsTest input1 cm1 $
                    mrm1
            , lookupResultsTest verInput1 cm1 $
                    mrm1
            ]
        , testsNonMatching
            [ lookupResultsTest input1 cm1 $
                    M.empty
            , lookupResultsTest verInput1 cm1 $
                    M.empty
            ]
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @app-misc/foo@, by itself, in @RDEPEND@
pd1 :: PkgDeps
pd1 = parseDepBlock "app-misc" "bar" "0.1"
    "app-misc/foo"

-- | Needs to be equivalent to @'buildCMap' 'pd1'@
cm1 :: ConstraintMap
cm1 = HM.singleton pkg1 urm1

urm1 :: UnevaluatedResultMap
urm1 = NEM.singleton revDep1 dep1

mrm1 :: EvaluatedResultMap 'Matching
mrm1 =
    evalResultMap
    (== depSpec1)
    urm1

-- @app-misc/foo@ with context in the 'ConstraintMap'
dep1 :: UnwrappedMap 'Nothing
dep1 = NEM.singleton RDEPEND ucm1

ucm1 :: ContextMap 'Nothing
ucm1 = NormalCtxMap $ NEM.singleton Nothing udm1

udm1 :: DepMap 'Nothing
udm1 = DepMap $ NEM.singleton depSpec1 ()

rcm1 :: ContextMap ('Just 'Matching)
rcm1 = NormalCtxMap $ NEM.singleton Nothing rdm1

rdm1 :: DepMap ('Just 'Matching)
rdm1 = DepMap $ NEM.singleton depSpec1 (All True)

-- | 'DepSpec' of @app-misc/foo@, which @app-misc/bar@ depends on
depSpec1 :: DepSpec
depSpec1 = UnversionedDepSpec
    Nothing
    pkg1
    Nothing
    Nothing

-- |
pkg1 :: Package
pkg1 = Package (Category "app-misc") (PkgName "foo")

revDep1 :: PkgWithVer
revDep1 = PkgWithVer
    (Package (Category "app-misc") (PkgName "bar"))
    (Version
        (VersionNum (NE.singleton '0' NE.:| [NE.singleton '1']))
        Nothing
        []
        Nothing
    )

input1 :: String
input1 = "app-misc/foo"

verInput1 :: String
verInput1 = "app-misc/foo-0.2"

-- EXAMPLE 2 --

example2 :: TestTree
example2 = testGroup "example 2"
    [ testCase "parsing" $ buildCMap pd2 @?= cm2
    , testGroup "isSpecRelevant"
        [ testsMatching
            [ isSpecRelevantTest input1 depSpecLower2 True
            , isSpecRelevantTest input1 depSpecUpper2 True
            , isSpecRelevantTest verInputMatching2 depSpecLower2 True
            , isSpecRelevantTest verInputMatching2 depSpecUpper2 True
            , isSpecRelevantTest verInputNonMatching2 depSpecLower2 True
            , isSpecRelevantTest verInputNonMatching2 depSpecUpper2 False
            ]
        , testsNonMatching
            [ isSpecRelevantTest input1 depSpecLower2 False
            , isSpecRelevantTest input1 depSpecUpper2 False
            , isSpecRelevantTest verInputMatching2 depSpecLower2 False
            , isSpecRelevantTest verInputMatching2 depSpecUpper2 False
            , isSpecRelevantTest verInputNonMatching2 depSpecLower2 False
            , isSpecRelevantTest verInputNonMatching2 depSpecUpper2 True
            ]
        ]
    , testGroup "evalDepMap"
        [ testsMatching
            [ evalDepMapTest input1 udm2 (Just rdm_verInputMatching2)
            , evalDepMapTest verInputMatching2 udm2 (Just rdm_verInputMatching2)
            , evalDepMapTest verInputNonMatching2 udm2 Nothing
            ]
        , testsNonMatching
            [ evalDepMapTest input1 udm2 Nothing
            , evalDepMapTest verInputMatching2 udm2 Nothing
            , evalDepMapTest verInputNonMatching2 udm2 (Just rdm_verInputNonMatching2)
            ]
        ]
    , testGroup "evalContextMap"
        [ testsMatching
            [ evalContextMapTest input1 ucm2 (Just rcm_verInputMatching2)
            , evalContextMapTest verInputMatching2 ucm2 (Just rcm_verInputMatching2)
            , evalContextMapTest verInputNonMatching2 ucm2 Nothing
            ]
        , testsNonMatching
            [ evalContextMapTest input1 ucm2 Nothing
            , evalContextMapTest verInputMatching2 ucm2 Nothing
            , evalContextMapTest verInputNonMatching2 ucm2 (Just rcm_verInputNonMatching2)
            ]
        ]
    , testGroup "lookupResults"
        [ testsMatching
            [ lookupResultsTest input1 cm2 $
                    rm_verInputMatching2
            , lookupResultsTest verInputMatching2 cm2 $
                    rm_verInputMatching2
            , lookupResultsTest verInputNonMatching2 cm2 $
                    M.empty
            ]
        , testsNonMatching
            [ lookupResultsTest input1 cm2 $
                    M.empty
            , lookupResultsTest verInputMatching2 cm2 $
                    M.empty
            , lookupResultsTest verInputNonMatching2 cm2 $
                    rm_verInputNonMatching2
            ]
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @>=app-misc/foo-0.1@, and @<app-misc/foo-0.2@,
--   in @RDEPEND@
pd2 :: PkgDeps
pd2 = parseDepBlock "app-misc" "bar" "0.1"
    ">=app-misc/foo-0.1 <app-misc/foo-0.2"

-- | Needs to be equivalent to @'buildCMap' 'pd2'@
cm2 :: ConstraintMap
cm2 = HM.singleton pkg1 urm2

urm2 :: UnevaluatedResultMap
urm2
    = NEM.singleton revDep1
    $ NEM.singleton RDEPEND
    $ ucm2

ucm2 :: ContextMap 'Nothing
ucm2 = NormalCtxMap $ NEM.singleton Nothing udm2

udm2 :: DepMap 'Nothing
udm2
    = DepMap $ NEM.fromList
    $ (depSpecLower2, ()) NE.:| [(depSpecUpper2, ())]

-- | Matches 'verInputMatching2' (@"app-misc/foo-0.1.1"@)
rm_verInputMatching2 :: EvaluatedResultMap 'Matching
rm_verInputMatching2
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputMatching2

rcm_verInputMatching2 :: ContextMap ('Just Matching)
rcm_verInputMatching2
    = NormalCtxMap $ NEM.singleton Nothing rdm_verInputMatching2

rdm_verInputMatching2 :: DepMap ('Just 'Matching)
rdm_verInputMatching2
    = DepMap $ NEM.fromList
    $ (depSpecLower2, All True) NE.:| [(depSpecUpper2, All True)]

-- | Matches 'verInputNonMatching2' (@"app-misc/foo-0.2"@)
rm_verInputNonMatching2 :: EvaluatedResultMap 'NonMatching
rm_verInputNonMatching2
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputNonMatching2

rcm_verInputNonMatching2 :: ContextMap ('Just 'NonMatching)
rcm_verInputNonMatching2
    = NormalCtxMap $ NEM.singleton Nothing rdm_verInputNonMatching2

rdm_verInputNonMatching2 :: DepMap ('Just 'NonMatching)
rdm_verInputNonMatching2
    = DepMap $ NEM.singleton depSpecUpper2 (Any True)

-- | 'DepSpec' of @>=app-misc/foo-0.1@, which @app-misc/bar@ depends on
depSpecLower2 :: DepSpec
depSpecLower2 = VersionedDepSpec
    Nothing
    (VPkgGE
        (Package (Category "app-misc") (PkgName "foo"))
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '1']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

-- | 'DepSpec' of @<app-misc/foo-0.2@, which @app-misc/bar@ depends on
depSpecUpper2 :: DepSpec
depSpecUpper2 = VersionedDepSpec
    Nothing
    (VPkgLT
        (Package (Category "app-misc") (PkgName "foo"))
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '2']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

verInputMatching2 :: String
verInputMatching2 = "app-misc/foo-0.1.1"

verInputNonMatching2 :: String
verInputNonMatching2 = "app-misc/foo-0.2"

-- EXAMPLE 3 --

example3 :: TestTree
example3 = testGroup "example 3"
    [ testCase "parsing" $ buildCMap pd3 @?= cm3
    , testGroup "isSpecRelevant"
        [ testsMatching
            [ isSpecRelevantTest input1 depSpecLower3 False
            , isSpecRelevantTest input1 depSpecUpper3 False
            , isSpecRelevantTest verInputMatching2 depSpecLower3 False
            , isSpecRelevantTest verInputMatching2 depSpecUpper3 False
            , isSpecRelevantTest verInputNonMatching2 depSpecLower3 False
            , isSpecRelevantTest verInputNonMatching2 depSpecUpper3 False
            , isSpecRelevantTest verInputMatching3 depSpecLower2 False
            , isSpecRelevantTest verInputMatching3 depSpecUpper2 False
            , isSpecRelevantTest verInputMatching3 depSpecLower3 True
            , isSpecRelevantTest verInputMatching3 depSpecUpper3 True
            , isSpecRelevantTest verInputNonMatching3 depSpecLower2 False
            , isSpecRelevantTest verInputNonMatching3 depSpecUpper2 False
            , isSpecRelevantTest verInputNonMatching3 depSpecLower3 False
            , isSpecRelevantTest verInputNonMatching3 depSpecUpper3 True
            ]
        , testsNonMatching
            [ isSpecRelevantTest input1 depSpecLower3 False
            , isSpecRelevantTest input1 depSpecUpper3 False
            , isSpecRelevantTest verInputMatching2 depSpecLower3 False
            , isSpecRelevantTest verInputMatching2 depSpecUpper3 False
            , isSpecRelevantTest verInputNonMatching2 depSpecLower3 False
            , isSpecRelevantTest verInputNonMatching2 depSpecUpper3 False
            , isSpecRelevantTest verInputMatching3 depSpecLower2 False
            , isSpecRelevantTest verInputMatching3 depSpecUpper2 False
            , isSpecRelevantTest verInputMatching3 depSpecLower3 False
            , isSpecRelevantTest verInputMatching3 depSpecUpper3 False
            , isSpecRelevantTest verInputNonMatching3 depSpecLower2 False
            , isSpecRelevantTest verInputNonMatching3 depSpecUpper2 False
            , isSpecRelevantTest verInputNonMatching3 depSpecLower3 True
            , isSpecRelevantTest verInputNonMatching3 depSpecUpper3 False
            ]
        ]
    , testGroup "evalDepMap"
        [ testsMatching
            [ evalDepMapTest input1 udm3 Nothing
            , evalDepMapTest verInputMatching2 udm3 Nothing
            , evalDepMapTest verInputNonMatching2 udm3 Nothing
            , evalDepMapTest verInputMatching3 udm3 (Just rdm_verInputMatching3)
            , evalDepMapTest verInputNonMatching3 udm3 Nothing
            ]
            , testsNonMatching
            [ evalDepMapTest input1 udm3 Nothing
            , evalDepMapTest verInputMatching2 udm3 Nothing
            , evalDepMapTest verInputNonMatching2 udm3 Nothing
            , evalDepMapTest verInputMatching3 udm3 Nothing
            , evalDepMapTest verInputNonMatching3 udm3 (Just rdm_verInputNonMatching3)
            ]
        ]
    , testGroup "evalContextMap"
        [ testsMatching
            [ evalContextMapTest input1 ucm3 Nothing
            , evalContextMapTest verInputMatching2 ucm3 Nothing
            , evalContextMapTest verInputNonMatching2 ucm3 Nothing
            , evalContextMapTest verInputMatching3 ucm3 (Just rcm_verInputMatching3)
            , evalContextMapTest verInputNonMatching3 ucm3 Nothing
            ]
        , testsNonMatching
            [ evalContextMapTest input1 ucm3 Nothing
            , evalContextMapTest verInputMatching2 ucm3 Nothing
            , evalContextMapTest verInputNonMatching2 ucm3 Nothing
            , evalContextMapTest verInputMatching2 ucm2 Nothing
            , evalContextMapTest verInputNonMatching2 ucm2 (Just rcm_verInputNonMatching2)
            ]
        ]
    , testGroup "lookupResults"
        [ testsMatching
            [ lookupResultsTest input1 cm3 $
                    rm_verInputMatching2
            , lookupResultsTest verInputMatching2 cm3 $
                    rm_verInputMatching2
            , lookupResultsTest verInputNonMatching2 cm3 $
                    M.empty
            , lookupResultsTest verInputMatching3 cm3 $
                    rm_verInputMatching3
            , lookupResultsTest verInputNonMatching3 cm3 $
                    M.empty
            ]
        , testsNonMatching
            [ lookupResultsTest input1 cm3 $
                    M.empty
            , lookupResultsTest verInputMatching2 cm3 $
                    M.empty
            , lookupResultsTest verInputNonMatching2 cm3 $
                    rm_verInputNonMatching2
            , lookupResultsTest verInputMatching3 cm3 $
                    M.empty
            , lookupResultsTest verInputNonMatching3 cm3 $
                    rm_verInputNonMatching3
            ]
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @>=app-misc/foo-0.1@, and @<app-misc/foo-0.2@,
--   as well as a conditional @>=app-misc/baz-0.3@ and @<app-misc/baz-0.4@ for
--   the @baz@ USE flag.
pd3 :: PkgDeps
pd3 = parseDepBlock "app-misc" "bar" "0.1"
    ">=app-misc/foo-0.1 <app-misc/foo-0.2 baz? ( >=app-misc/baz-0.3 <app-misc/baz-0.4 )"

-- | Needs to be equivalent to @'buildCMap' 'pd3'@
cm3 :: ConstraintMap
cm3 = HM.fromList
    [ (pkg1, urm2)
    , (pkg3, urm3)
    ]

urm3 :: UnevaluatedResultMap
urm3
    = NEM.singleton revDep1
    $ NEM.singleton RDEPEND
    $ ucm3

ucm3 :: ContextMap 'Nothing
ucm3 = NormalCtxMap $ NEM.singleton (Just depCtx3) udm3

udm3 :: DepMap 'Nothing
udm3
    = DepMap $ NEM.fromList
    $ (depSpecLower3, ()) NE.:| [(depSpecUpper3, ())]

rm_verInputMatching3 :: EvaluatedResultMap 'Matching
rm_verInputMatching3
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputMatching3

rcm_verInputMatching3 :: ContextMap ('Just 'Matching)
rcm_verInputMatching3
    = NormalCtxMap $ NEM.singleton (Just depCtx3) rdm_verInputMatching3

rdm_verInputMatching3 :: DepMap ('Just 'Matching)
rdm_verInputMatching3
    = DepMap $ NEM.fromList
    $ (depSpecLower3, All True) NE.:| [(depSpecUpper3, All True)]

rm_verInputNonMatching3 :: EvaluatedResultMap 'NonMatching
rm_verInputNonMatching3
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputNonMatching3

rcm_verInputNonMatching3 :: ContextMap ('Just 'NonMatching)
rcm_verInputNonMatching3
    = NormalCtxMap $ NEM.singleton (Just depCtx3) rdm_verInputNonMatching3

rdm_verInputNonMatching3 :: DepMap ('Just 'NonMatching)
rdm_verInputNonMatching3
    = DepMap $ NEM.singleton depSpecLower3 (Any True)

-- | Carries the context of
--
--   @
--   baz? ( >=app-misc/baz-0.3 <app-misc/baz-0.4 )
--   @
depCtx3 :: DepContext
depCtx3 =
    UseCtx
    (NE.fromList
        [ Right depSpecLower3
        , Right depSpecUpper3
        ]
    )
    (UseFlag "baz")

-- | 'DepSpec' of @>=app-misc/baz-0.3@, which @app-misc/bar@ depends on
depSpecLower3 :: DepSpec
depSpecLower3 = VersionedDepSpec
    Nothing
    (VPkgGE
        (Package (Category "app-misc") (PkgName "baz"))
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '3']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

-- | 'DepSpec' of @<app-misc/baz-0.4@, which @app-misc/bar@ depends on (needed by
--   'isDepRelevant')
depSpecUpper3 :: DepSpec
depSpecUpper3 = VersionedDepSpec
    Nothing
    (VPkgLT
        (Package (Category "app-misc") (PkgName "baz"))
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '4']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

pkg3 :: Package
pkg3 = Package (Category "app-misc") (PkgName "baz")

verInputMatching3 :: String
verInputMatching3 = "app-misc/baz-0.3"

verInputNonMatching3 :: String
verInputNonMatching3 = "app-misc/baz-0.2"

-- OR LOGIC --

example_Or :: TestTree
example_Or = testGroup "Or groups"
    [ testCase "parsing" $ buildCMap pd_Or @?= cm_Or
    , testGroup "isSpecRelevant"
        [ testsMatching
            [ isSpecRelevantTest input1 depSpecFirst_Or True
            , isSpecRelevantTest input1 depSpecSecond_Or True
            , isSpecRelevantTest verInputMatchingLower_Or depSpecFirst_Or True
            , isSpecRelevantTest verInputMatchingLower_Or depSpecSecond_Or False
            , isSpecRelevantTest verInputMatchingUpper_Or depSpecFirst_Or False
            , isSpecRelevantTest verInputMatchingUpper_Or depSpecSecond_Or True
            , isSpecRelevantTest verInputNonMatching_Or depSpecFirst_Or False
            , isSpecRelevantTest verInputNonMatching_Or depSpecSecond_Or False
            ]
        , testsNonMatching
            [ isSpecRelevantTest input1 depSpecFirst_Or False
            , isSpecRelevantTest input1 depSpecSecond_Or False
            , isSpecRelevantTest verInputMatchingLower_Or depSpecFirst_Or False
            , isSpecRelevantTest verInputMatchingLower_Or depSpecSecond_Or True
            , isSpecRelevantTest verInputMatchingUpper_Or depSpecFirst_Or True
            , isSpecRelevantTest verInputMatchingUpper_Or depSpecSecond_Or False
            , isSpecRelevantTest verInputNonMatching_Or depSpecFirst_Or True
            , isSpecRelevantTest verInputNonMatching_Or depSpecSecond_Or True
            ]
        ]
    , testGroup "evalDepMap"
        [ testsMatching
            [ evalDepMapTest input1 udm_Or (Just rdm_input1_Or)
            , evalDepMapTest verInputMatchingLower_Or udm_Or (Just rdm_verInputMatchingLower_Or)
            , evalDepMapTest verInputMatchingUpper_Or udm_Or (Just rdm_verInputMatchingUpper_Or)
            , evalDepMapTest verInputNonMatching_Or udm_Or Nothing
            ]
        , testsNonMatching
            [ evalDepMapTest input1 udm_Or Nothing
            , evalDepMapTest verInputMatchingLower_Or udm_Or Nothing
            , evalDepMapTest verInputMatchingUpper_Or udm_Or Nothing
            , evalDepMapTest verInputNonMatching_Or udm_Or (Just rdm_verInputNonMatching_Or)
            ]
        ]
    , testGroup "evalContextMap"
        [ testsMatching
            [ evalContextMapTest input1 ucm_Or (Just rcm_input1_Or)
            , evalContextMapTest verInputMatchingLower_Or ucm_Or (Just rcm_verInputMatchingLower_Or)
            , evalContextMapTest verInputMatchingUpper_Or ucm_Or (Just rcm_verInputMatchingUpper_Or)
            , evalContextMapTest verInputNonMatching_Or ucm_Or Nothing
            ]
        , testsNonMatching
            [ evalContextMapTest input1 ucm_Or Nothing
            , evalContextMapTest verInputMatchingLower_Or ucm_Or Nothing
            , evalContextMapTest verInputMatchingUpper_Or ucm_Or Nothing
            , evalContextMapTest verInputNonMatching_Or ucm_Or (Just rcm_verInputNonMatching_Or)
            ]
        ]
    , testGroup "lookupResults"
        [ testsMatching
            [ lookupResultsTest input1 cm_Or $
                    rm_input1_Or
            , lookupResultsTest verInputMatchingLower_Or cm_Or $
                    rm_verInputMatchingLower_Or
            , lookupResultsTest verInputMatchingUpper_Or cm_Or $
                    rm_verInputMatchingUpper_Or
            , lookupResultsTest verInputNonMatching_Or cm_Or $
                    M.empty
            ]
        , testsNonMatching
            [ lookupResultsTest input1 cm_Or $
                    M.empty
            , lookupResultsTest verInputMatchingLower_Or cm_Or $
                    M.empty
            , lookupResultsTest verInputMatchingUpper_Or cm_Or $
                    M.empty
            , lookupResultsTest verInputNonMatching_Or cm_Or $
                    rm_verInputNonMatching_Or
            ]
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @<app-misc/foo-0.3@, /or/ @>=app-misc/foo-0.4@,
--   effectively including all versions of @app-misc/foo@ /except/ @=app-misc/foo-0.3.*@.
pd_Or :: PkgDeps
pd_Or = parseDepBlock "app-misc" "bar" "0.1"
    "|| ( <app-misc/foo-0.3 >=app-misc/foo-0.4 )"

-- | Needs to be equivalent to @'buildCMap' 'pd_Or'@
cm_Or :: ConstraintMap
cm_Or = HM.singleton pkg1 urm_Or

urm_Or :: UnevaluatedResultMap
urm_Or
    = NEM.singleton revDep1
    $ NEM.singleton RDEPEND
    $ ucm_Or

ucm_Or :: ContextMap 'Nothing
ucm_Or = OrGroupCtxMap $ NEM.singleton depCtx_Or udm_Or

udm_Or :: OrGroupMap 'Nothing
udm_Or
    = OrGroupMap $ NEM.fromList
    $ (depSpecFirst_Or, ()) NE.:| [(depSpecSecond_Or, ())]

rm_input1_Or :: EvaluatedResultMap 'Matching
rm_input1_Or
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_input1_Or

rcm_input1_Or :: ContextMap ('Just 'Matching)
rcm_input1_Or
    = OrGroupCtxMap $ NEM.singleton depCtx_Or rdm_input1_Or

rdm_input1_Or :: OrGroupMap ('Just Matching)
rdm_input1_Or
    = OrGroupMap $ NEM.fromList
    $ (depSpecFirst_Or, Any True) NE.:| [(depSpecSecond_Or, Any True)]

rm_verInputMatchingLower_Or :: EvaluatedResultMap 'Matching
rm_verInputMatchingLower_Or
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputMatchingLower_Or

rcm_verInputMatchingLower_Or :: ContextMap ('Just Matching)
rcm_verInputMatchingLower_Or
    = OrGroupCtxMap $ NEM.singleton depCtx_Or rdm_verInputMatchingLower_Or

rdm_verInputMatchingLower_Or :: OrGroupMap ('Just Matching)
rdm_verInputMatchingLower_Or
    = OrGroupMap $ NEM.singleton depSpecFirst_Or (Any True)

rm_verInputMatchingUpper_Or :: EvaluatedResultMap 'Matching
rm_verInputMatchingUpper_Or
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputMatchingUpper_Or

rcm_verInputMatchingUpper_Or :: ContextMap ('Just 'Matching)
rcm_verInputMatchingUpper_Or
    = OrGroupCtxMap $ NEM.singleton depCtx_Or rdm_verInputMatchingUpper_Or

rdm_verInputMatchingUpper_Or :: OrGroupMap ('Just 'Matching)
rdm_verInputMatchingUpper_Or
    = OrGroupMap $ NEM.singleton depSpecSecond_Or (Any True)

rm_verInputNonMatching_Or :: EvaluatedResultMap 'NonMatching
rm_verInputNonMatching_Or
    = M.singleton revDep1
    $ NEM.singleton RDEPEND rcm_verInputNonMatching_Or

rcm_verInputNonMatching_Or :: ContextMap ('Just 'NonMatching)
rcm_verInputNonMatching_Or
    = OrGroupCtxMap $ NEM.singleton depCtx_Or rdm_verInputNonMatching_Or

rdm_verInputNonMatching_Or :: OrGroupMap ('Just 'NonMatching)
rdm_verInputNonMatching_Or
    = OrGroupMap $ NEM.fromList
    $ (depSpecFirst_Or, All True) NE.:| [(depSpecSecond_Or, All True)]

depSpecFirst_Or :: DepSpec
depSpecFirst_Or = VersionedDepSpec
    Nothing
    (VPkgLT
        pkg1
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '3']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

depSpecSecond_Or :: DepSpec
depSpecSecond_Or = VersionedDepSpec
    Nothing
    (VPkgGE
        pkg1
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '4']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

depCtx_Or :: OrContext
depCtx_Or =
    OrCtx
    (NE.fromList
        [ Right depSpecFirst_Or
        , Right depSpecSecond_Or
        ]
    )

verInputMatchingLower_Or :: String
verInputMatchingLower_Or = "app-misc/foo-0.2"

verInputMatchingUpper_Or :: String
verInputMatchingUpper_Or = "app-misc/foo-0.4"

verInputNonMatching_Or :: String
verInputNonMatching_Or = "app-misc/foo-0.3"
---

testsMatching :: [LiftedMatchMode 'Matching -> TestTree] -> TestTree
testsMatching ts = testGroup "Matching" $ ts <*> [LMatching]

testsNonMatching :: [LiftedMatchMode 'NonMatching -> TestTree] -> TestTree
testsNonMatching ts = testGroup "NonMatching" $ ts <*> [LNonMatching]

lookupResultsTest
    :: AllDepMaps m '[ IsBool ]
    => String
    -> ConstraintMap
    -> EvaluatedResultMap m
    -> LiftedMatchMode m
    -> TestTree
lookupResultsTest inStr cmap ex mode
    = testCase (show inStr)
        $ lookupResults mode (inputString inStr) cmap
            @?= ex

inputString :: String -> CmdlinePkg
inputString = validation (error . show) id . parsePkg

isSpecRelevantTest :: String -> DepSpec -> Bool -> LiftedMatchMode m -> TestTree
isSpecRelevantTest inStr spec ex mode
    = testCase msg
    $ let res = isSpecRelevant mode (inputString inStr) spec
      in  case ex of
            True -> res @? inStr ++ " did not match " ++ toString spec
            False -> not res @? inStr ++ " matches " ++ toString spec
  where
    msg = show inStr ++ mch ++ toString spec
    mch | ex = " should match "
        | otherwise = " should not match "

evalDepMapTest
    :: ( IsDepMap t
       , Show (t 'Nothing)
       , Show (t ('Just m))
       , Eq (t ('Just m))
       , IsBool (MatchLogic t ('Just m)) )
    => String
    -> t 'Nothing
    -> Maybe (t ('Just m))
    -> LiftedMatchMode m
    -> TestTree
evalDepMapTest inStr dm ex mode = testCase msg
    $ evalDepMap (isSpecRelevant mode (inputString inStr)) dm @?= ex
  where
    msg = show inStr

evalContextMapTest
    :: AllDepMaps m '[ IsBool ]
    => String
    -> ContextMap 'Nothing
    -> Maybe (ContextMap ('Just m))
    -> LiftedMatchMode m
    -> TestTree
evalContextMapTest inStr cm ex mode = testCase msg
    $ evalContextMap (isSpecRelevant mode (inputString inStr)) cm @?= ex
  where
    msg = show inStr

-- | Generate a 'PkgDeps' by parsing a 'DepBlock'. This adds
--   all dep specs to @RDEPEND@.
parseDepBlock
    :: String -- ^ 'Category'
    -> String -- ^ 'PkgName'
    -> String -- ^ 'Version'
    -> String -- ^ 'DepBlock'
    -> PkgDeps
parseDepBlock cat pkg v b
    = PkgDeps
        (Package (Category cat) (PkgName pkg), ver, Slot "0" Nothing)
        []
        blk
        []
        []
        []
  where
    blk :: DepBlock
    blk = case runParsable (encodeString b) of
        Right b' -> b'
        Left e -> error $ "Could not parse DepBlock " ++ show b ++ ": " ++ show e

    ver :: Version
    ver = case runParsable (encodeString v) of
        Right v' -> v'
        Left e -> error $ "Could not parse Version " ++ show v ++ ": " ++ show e


type UnwrappedMap m = NEMap DepVar (ContextMap m)

