{-# Language DataKinds #-}
{-# Language LambdaCase #-}
{-# Language TypeFamilies #-}

module RevdepScanner.Logic
    ( lookupResults
    ) where

import qualified Data.HashMap.Strict as HM

import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.DepSet
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
    :: forall m.
        ( Monoid (MatchLogic DepSet' ('Just m))
        , Monoid (MatchLogic DepOrGroup' ('Just m))
        , Ord (MatchLogic DepSet' ('Just m))
        , Ord (MatchLogic DepOrGroup' ('Just m))
        )
    => LiftedMatchMode m
    -> CmdlinePkg
    -> ConstraintMap
    -> EvaluatedResultMap m
lookupResults mode cp
    = foldMap (evalResultMap mode isSpecRelevant)
    . HM.lookup (cmdlinePkgPackage cp)
  where

    isSpecRelevant :: DepSpec -> Bool
    isSpecRelevant s =
        let b = case (cp, s) of
                (CmdlinePkgWithVer (PkgWithVer p v), VersionedDepSpec _ vp _ _)
                    -> matchVersionedPackage vp p v
                _ -> True

        -- NonMatching mode inverts the logic
        in case mode of
            LMatching -> b
            LNonMatching -> not b
