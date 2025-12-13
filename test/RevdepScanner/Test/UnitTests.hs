{-# Language OverloadedLists #-}

module RevdepScanner.Test.UnitTests
    ( simpleTests
    ) where

import Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
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
import RevdepScanner.Util (encodeString)
import qualified RevdepScanner.Types.HashSet.NonEmpty as NES

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
    [ testCase "parsing" $ buildCMap simplePD1 @?= simpleCM1
    , inputTestsMatching
        [ inputTest simpleInput1 simpleCM1 $
                simpleRM1
        , inputTest simpleVerInput1 simpleCM1 $
                simpleRM1
        ]
    , inputTestsNonMatching
        [ inputTest simpleInput1 simpleCM1 $
                M.empty
        , inputTest simpleVerInput1 simpleCM1 $
                M.empty
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @app-misc/foo@, by itself, in @RDEPEND@
simplePD1 :: PkgDeps
simplePD1 = parseDepBlock "app-misc" "bar" "0.1"
    "app-misc/foo"

-- | Needs to be equivalent to @'buildCMap' 'simplePD1'@
simpleCM1 :: ConstraintMap
simpleCM1 = M.singleton simplePkg1 simpleRM1

simpleRM1 :: ResultMap
simpleRM1 = M.singleton simpleRevDep1
    $ NES.singleton simpleDep1

-- @app-misc/foo@ with context in the 'ConstraintMap'
simpleDep1 :: DepWithCtx
simpleDep1 = DepWithCtx
    simpleDepSpec1
    RDEPEND
    Nothing

-- | 'DepSpec' of @app-misc/foo@, which @app-misc/bar@ depends on
simpleDepSpec1 :: DepSpec
simpleDepSpec1 = UnversionedDepSpec
    Nothing
    simplePkg1
    Nothing
    Nothing

-- |
simplePkg1 :: Package
simplePkg1 = Package (Category "app-misc") (PkgName "foo")

simpleRevDep1 :: PkgWithVer
simpleRevDep1 = PkgWithVer
    (Package (Category "app-misc") (PkgName "bar"))
    (Version
        (VersionNum (NE.singleton '0' NE.:| [NE.singleton '1']))
        Nothing
        []
        Nothing
    )

simpleInput1 :: String
simpleInput1 = "app-misc/foo"

simpleVerInput1 :: String
simpleVerInput1 = "app-misc/foo-0.2"

-- EXAMPLE 2 --

example2 :: TestTree
example2 = testGroup "example 2"
    [ testCase "parsing" $ buildCMap simplePD2 @?= simpleCM2
    , inputTestsMatching
        [ inputTest simpleInput1 simpleCM2 $
                simpleRM2
        , inputTest simpleVerInputMatching2 simpleCM2 $
                simpleRM2
        , inputTest simpleVerInputNonMatching2 simpleCM2 $
                M.empty
        ]
    , inputTestsNonMatching
        [ inputTest simpleInput1 simpleCM2 $
                M.empty
        , inputTest simpleVerInputMatching2 simpleCM2 $
                M.empty
        , inputTest simpleVerInputNonMatching2 simpleCM2 $
                simpleRM2
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @>=app-misc/foo-0.1@, and @<app-misc/foo-0.2@,
--   in @RDEPEND@
simplePD2 :: PkgDeps
simplePD2 = parseDepBlock "app-misc" "bar" "0.1"
    ">=app-misc/foo-0.1 <app-misc/foo-0.2"

-- | Needs to be equivalent to @'buildCMap' 'simplePD2'@
simpleCM2 :: ConstraintMap
simpleCM2 = M.singleton simplePkg1 simpleRM2

simpleRM2 :: ResultMap
simpleRM2 = M.singleton simpleRevDep1
    $ fromJust $ NES.fromList
    [ simpleDepLower2
    , simpleDepUpper2
    ]

-- | Lower constraint of @app-misc/foo@ with context in the 'ConstraintMap'
simpleDepLower2 :: DepWithCtx
simpleDepLower2 = DepWithCtx
    simpleDepSpecLower2
    RDEPEND
    Nothing

-- | 'DepSpec' of @>=app-misc/foo-0.1@, which @app-misc/bar@ depends on
simpleDepSpecLower2 :: DepSpec
simpleDepSpecLower2 = VersionedDepSpec
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

-- | Upper constraint of @app-misc/foo@ with context in the 'ConstraintMap'
simpleDepUpper2 :: DepWithCtx
simpleDepUpper2 = DepWithCtx
    simpleDepSpecUpper2
    RDEPEND
    Nothing

-- | 'DepSpec' of @<app-misc/foo-0.2@, which @app-misc/bar@ depends on
simpleDepSpecUpper2 :: DepSpec
simpleDepSpecUpper2 = VersionedDepSpec
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

simpleVerInputMatching2 :: String
simpleVerInputMatching2 = "app-misc/foo-0.1.1"

simpleVerInputNonMatching2 :: String
simpleVerInputNonMatching2 = "app-misc/foo-0.2"

-- EXAMPLE 3 --

example3 :: TestTree
example3 = testGroup "example 3"
    [ testCase "parsing" $ buildCMap simplePD3 @?= simpleCM3
    , inputTestsMatching
        [ inputTest simpleInput1 simpleCM3 $
                M.singleton simpleRevDep1 -- "app-misc/foo"
                    (fromJust $ NES.fromList
                        [ simpleDepLower2 -- ">=app-misc/foo-0.1"
                        , simpleDepUpper2 -- ">=app-misc/foo-0.2"
                        ]
                    )
        , inputTest simpleVerInputMatching2 simpleCM3 $
                simpleRM2
        , inputTest simpleVerInputNonMatching2 simpleCM3 $
                M.empty
        , inputTest simpleVerInputMatching3 simpleCM3 $
                simpleRM3
        , inputTest simpleVerInputNonMatching3 simpleCM3 $
                M.empty
        ]
    , inputTestsNonMatching
        [ inputTest simpleInput1 simpleCM3 $
                M.empty
        , inputTest simpleVerInputMatching2 simpleCM3 $
                M.empty
        , inputTest simpleVerInputNonMatching2 simpleCM3 $
                simpleRM2
        , inputTest simpleVerInputMatching3 simpleCM3 $
                M.empty
        , inputTest simpleVerInputNonMatching3 simpleCM3 $
                simpleRM3
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @>=app-misc/foo-0.1@, and @<app-misc/foo-0.2@,
--   as well as a conditional @>=app-misc/baz-0.3@ and @<app-misc/baz-0.4@ for
--   the @baz@ USE flag.
simplePD3 :: PkgDeps
simplePD3 = parseDepBlock "app-misc" "bar" "0.1"
    ">=app-misc/foo-0.1 <app-misc/foo-0.2 baz? ( >=app-misc/baz-0.3 <app-misc/baz-0.4 )"

-- | Needs to be equivalent to @'buildCMap' 'simplePD3'@
simpleCM3 :: ConstraintMap
simpleCM3 = M.fromList
    [ (simplePkg1, simpleRM2)
    , (simplePkg3, simpleRM3)
    ]
  where
    simplePkg3 :: Package
    simplePkg3 = Package (Category "app-misc") (PkgName "baz")

simpleRM3 :: ResultMap
simpleRM3 = M.singleton simpleRevDep1
    $ fromJust $ NES.fromList
    [ simpleDepLower3
    , simpleDepUpper3
    ]

-- | Lower constraint of @app-misc/baz@ with context in the 'ConstraintMap'
simpleDepLower3 :: DepWithCtx
simpleDepLower3 = DepWithCtx
    simpleDepSpecLower3
    RDEPEND
    (Just simpleDepCtx3)

-- | Carries the context of
--
--   @
--   baz? ( >=app-misc/baz-0.3 <app-misc/baz-0.4 )
--   @
simpleDepCtx3 :: DepContext
simpleDepCtx3 =
    UseCtx
    (NE.fromList
        [ Right simpleDepSpecLower3
        , Right simpleDepSpecUpper3
        ]
    )
    (UseFlag "baz")

-- | 'DepSpec' of @>=app-misc/baz-0.3@, which @app-misc/bar@ depends on
simpleDepSpecLower3 :: DepSpec
simpleDepSpecLower3 = VersionedDepSpec
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

-- | Upper constraint of @app-misc/foo@ with context in the 'ConstraintMap'
simpleDepUpper3 :: DepWithCtx
simpleDepUpper3 = DepWithCtx
    simpleDepSpecUpper3
    RDEPEND
    (Just simpleDepCtx3)

-- | 'DepSpec' of @<app-misc/baz-0.4@, which @app-misc/bar@ depends on (needed by
--   'isDepRelevant')
simpleDepSpecUpper3 :: DepSpec
simpleDepSpecUpper3 = VersionedDepSpec
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

simpleVerInputMatching3 :: String
simpleVerInputMatching3 = "app-misc/baz-0.3"

simpleVerInputNonMatching3 :: String
simpleVerInputNonMatching3 = "app-misc/baz-0.2"

-- OR LOGIC --

example_Or :: TestTree
example_Or = testGroup "Or groups"
    [ testCase "parsing" $ buildCMap simplePD_Or @?= simpleCM_Or
    , inputTestsMatching
        [ inputTest simpleInput1 simpleCM_Or $
                simpleRM_Or
        , inputTest simpleVerInputMatchingLower_Or simpleCM_Or $
                M.singleton simpleRevDep1
                    $ NES.singleton simpleDepFirst_Or
        , inputTest simpleVerInputMatchingUpper_Or simpleCM_Or $
                M.singleton simpleRevDep1
                    $ NES.singleton simpleDepSecond_Or
        , inputTest simpleVerInputNonMatching_Or simpleCM_Or $
                M.empty
        ]
    , inputTestsNonMatching
        [ inputTest simpleInput1 simpleCM_Or $
                M.empty
        , inputTest simpleVerInputMatchingLower_Or simpleCM_Or $
                M.empty
        , inputTest simpleVerInputMatchingUpper_Or simpleCM_Or $
                M.empty
        , inputTest simpleVerInputNonMatching_Or simpleCM_Or $
                simpleRM_Or
        ]
    ]

-- | @app-misc/bar-0.1@ depends on @<app-misc/foo-0.3@, /or/ @>=app-misc/foo-0.4@,
--   effectively including all versions of @app-misc/foo@ /except/ @=app-misc/foo-0.3.*@.
simplePD_Or :: PkgDeps
simplePD_Or = parseDepBlock "app-misc" "bar" "0.1"
    "|| ( <app-misc/foo-0.3 >=app-misc/foo-0.4 )"

-- | Needs to be equivalent to @'buildCMap' 'simplePD_Or'@
simpleCM_Or :: ConstraintMap
simpleCM_Or = M.singleton simplePkg1 simpleRM_Or

simpleRM_Or :: ResultMap
simpleRM_Or = M.singleton simpleRevDep1
    $ fromJust $ NES.fromList
        [ simpleDepFirst_Or
        , simpleDepSecond_Or
        ]

simpleDepFirst_Or :: DepWithCtx
simpleDepFirst_Or = DepWithCtx
    simpleDepSpecFirst_Or
    RDEPEND
    (Just simpleDepCtx_Or)

simpleDepSecond_Or :: DepWithCtx
simpleDepSecond_Or = DepWithCtx
    simpleDepSpecSecond_Or
    RDEPEND
    (Just simpleDepCtx_Or)

simpleDepSpecFirst_Or :: DepSpec
simpleDepSpecFirst_Or = VersionedDepSpec
    Nothing
    (VPkgLT
        simplePkg1
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '3']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

simpleDepSpecSecond_Or :: DepSpec
simpleDepSpecSecond_Or = VersionedDepSpec
    Nothing
    (VPkgGE
        simplePkg1
        (Version
            (VersionNum (NE.singleton '0' NE.:| [NE.singleton '4']))
            Nothing
            []
            Nothing
        )
    )
    Nothing
    Nothing

simpleDepCtx_Or :: DepContext
simpleDepCtx_Or =
    OrCtx
    (NE.fromList
        [ Right simpleDepSpecFirst_Or
        , Right simpleDepSpecSecond_Or
        ]
    )

simpleVerInputMatchingLower_Or :: String
simpleVerInputMatchingLower_Or = "app-misc/foo-0.2"

simpleVerInputMatchingUpper_Or :: String
simpleVerInputMatchingUpper_Or = "app-misc/foo-0.4"

simpleVerInputNonMatching_Or :: String
simpleVerInputNonMatching_Or = "app-misc/foo-0.3"
---

inputTestsMatching :: [MatchMode -> TestTree] -> TestTree
inputTestsMatching ts = testGroup "Matching" $ ts <*> [Matching]

inputTestsNonMatching :: [MatchMode -> TestTree] -> TestTree
inputTestsNonMatching ts = testGroup "NonMatching" $ ts <*> [NonMatching]

inputTest
    :: String
    -> ConstraintMap
    -> ResultMap
    -> MatchMode
    -> TestTree
inputTest inStr cmap ex mode
    = testCase (show inStr)
        $ ppRMap (lookupResults mode (inputString inStr) cmap)
            @?= ppRMap ex
  where
    ppRMap :: ResultMap -> M.HashMap String [(String, String, String)]
    ppRMap = M.map (map ppDWC . toList) . M.mapKeys toString

    ppDWC :: DepWithCtx -> (String, String, String)
    ppDWC (DepWithCtx s v mc) = (toString s, show v, show (toString <$> mc))

inputString :: String -> CmdlinePkg
inputString = validation (error . show) id . parsePkg

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
