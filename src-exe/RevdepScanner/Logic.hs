module RevdepScanner.Logic
    ( lookupResults
    , isDepRelevant
    ) where

import qualified Data.HashMap.Strict as M

import Distribution.Portage.Types

import RevdepScanner.CmdLine
import RevdepScanner.Types
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
    :: MatchMode
    -> Either Package PkgWithVer
    -> ConstraintMap
    -> ResultMap
lookupResults mode ep =
    foldMap (M.filter (any check)) . M.lookup (either id pwvPackage ep)
  where
    check :: DepWithCtx -> Bool
    check = isDepRelevant mode ep . dwcDepSpec

-- | Check if a 'DepSpec' should be displayed, given the 'MatchMode' and
--   package (with optional version) from the command line.
isDepRelevant
    :: MatchMode
    -> Either Package PkgWithVer
    -> DepSpec
    -> Bool
isDepRelevant m0 e0 s0 =
    let b = case (e0, s0) of
            -- Ignore the DepSpec if it's a blocker
            (_, VersionedDepSpec (Just _) _ _ _) -> False
            (_, UnversionedDepSpec (Just _) _ _ _) -> False

            (Left p0, VersionedDepSpec _ vp _ _)
                -> p0 == vPkgPackage vp
            (Left p0, UnversionedDepSpec _ p _ _)
                -> p0 == p
            (Right (PkgWithVer p0 v0), VersionedDepSpec _ vp _ _)
                -> matchVersionedPackage vp p0 v0
            (Right (PkgWithVer p0 _), UnversionedDepSpec _ p _ _)
                -> p0 == p
    in case m0 of
            Matching -> b
            NonMatching -> not b
