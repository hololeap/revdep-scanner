{-# Language DataKinds #-}
{-# Language LambdaCase #-}
{-# Language TypeFamilies #-}

module RevdepScanner.Logic
    ( lookupResults
    , isSpecRelevant
    ) where

import qualified Data.HashMap.Strict as HM

import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.DepMap
import RevdepScanner.Types.ResultMap
import RevdepScanner.Types.ConstraintMap (ConstraintMap)

-- | Given the 'MatchMode' and a package given on the command line (either
--   'Package' or 'PkgWithVer'), look up a mapping of reverse dependencies
--   to dependencies in the 'ConstraintMap'.
--
--   For instance, looking up @dev-haskell/cabal@ would produce a mapping:
--
--   @
--   (revdep of dev-haskell/cabal e.g. 'PkgWithVer')
--       -> ( relevant dependencies containing dev-haskell/cabal
--            e.g. (HashSet ('DepSpec', 'DepVar', Maybe 'DepContext'))
--          )
--   @
lookupResults
    :: forall m. AllDepMaps m '[ IsBool ]
    => LiftedMatchMode m
    -> CmdlinePkg
    -> ConstraintMap
    -> EvaluatedResultMap m
lookupResults mode cp
    = foldMap (evalResultMap (isSpecRelevant mode cp))
    . HM.lookup (cmdlinePkgPackage cp)
  where

isSpecRelevant
    :: LiftedMatchMode m
    -> CmdlinePkg
    -> DepSpec
    -> Bool
isSpecRelevant mode cp s = case (mode, cp, s) of
    (LNonMatching, CmdlinePkgWithVer (PkgWithVer p v), VersionedDepSpec _ vp _ _)
        -> p == (vPkgPackage vp) && not (matchVersionedPackage vp p v)
    -- Non-matching mode really only makes sense for CmdlinePkgWithVer/VersionedDepSpec
    (LNonMatching, _, _) -> False
    (_, CmdlinePkgWithVer (PkgWithVer p v), VersionedDepSpec _ vp _ _)
        -> matchVersionedPackage vp p v
    (_, CmdlinePkgWithVer (PkgWithVer p _), UnversionedDepSpec _ p' _ _)
        -> p == p'
    (_, CmdlinePackage p, VersionedDepSpec _ vp _ _)
        -> p == vPkgPackage vp
    (_, CmdlinePackage p, UnversionedDepSpec _ p' _ _)
        -> p == p'
