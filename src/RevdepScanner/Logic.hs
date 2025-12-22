{-# Language DataKinds #-}
{-# Language DerivingVia #-}
{-# Language LambdaCase #-}
{-# Language TypeFamilies #-}

module RevdepScanner.Logic
    ( lookupResults
    , isSpecRelevant
    , buildCMap
    ) where

import Control.DeepSeq
import qualified Control.Foldl as Foldl
import           Control.Foldl (fold)
import Control.Monad.Reader
import qualified Data.HashMap.Monoidal as HM
import qualified Data.List.NonEmpty as NEL
import Data.Monoid

import Distribution.Portage.Types
import Distribution.Gentoo.Utils.Pquery

import RevdepScanner.Types
import qualified RevdepScanner.Types.ContextMap as CtxMap
import           RevdepScanner.Types.ContextMap (ContextMap)
import RevdepScanner.Types.DepMap
import           RevdepScanner.Types.NEMMap (NEMMap)
import qualified RevdepScanner.Types.NEMMap as NEM
import RevdepScanner.Types.ResultMap
import qualified RevdepScanner.Types.ConstraintMap as ConMap
import           RevdepScanner.Types.ConstraintMap (ConstraintMap)

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
    = fold (Foldl.foldMap (evalResultMap (isSpecRelevant mode cp)) id)
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

-- | Build a 'ConstraintMap' by scanning the contents of a 'PkgDeps' entry
buildCMap :: PkgDeps -> ConstraintMap
-- TODO: Should pquery/PkgDeps ignore the slot value completely?
buildCMap (PkgDeps (p0,v0,_) depBlock rdepBlock bdepBlock pdepBlock idepBlock) =
    -- Needs to find all packages referenced by any of the blocks, note
    -- which package the dep block lives in, and its name.
    let vars =
            [ (DEPEND, depBlock)
            , (RDEPEND, rdepBlock)
            , (BDEPEND, bdepBlock)
            , (PDEPEND, pdepBlock)
            , (IDEPEND, idepBlock)
            ]
        act :: (DepVar, DepBlock)
            -> Reader (First (Either DepContext OrContext)) ConstraintMap
        act (var, DepBlock blk)
            = Foldl.foldM (foldMap (finalize var) <$> Foldl.sink fromGroup) blk
    in  Foldl.fold (Foldl.foldMap (\v -> runReader (act v) mempty) id) vars
  where
    fromGroup
        :: Either DepGroup DepSpec
        -> Reader (First (Either DepContext OrContext)) (Maybe DepVarMap)
    fromGroup = \case
        Left g -> do
            case g of
                -- "And" groups are very basic. We don't need to remember them
                AndGroup ne -> foldMapM fromGroup ne
                -- The other groups are more complex and should be saved as
                -- context for the output
                OrGroup ne -> do
                    local (<> pure (Right (OrCtx ne)))
                        $ foldMapM fromGroup ne
                UseGroup ne u -> do
                    local (<> pure (Left (UseCtx ne u)))
                        $ foldMapM fromGroup ne
                NotUseGroup ne u -> do
                    local (<> pure (Left (NotUseCtx ne u)))
                        $ foldMapM fromGroup ne
        Right s
            | isBlocker s -> pure Nothing -- Skip blockers
            | otherwise -> Just <$> do
                let p = case s of
                        VersionedDepSpec _ vp _ _ -> vPkgPackage vp
                        UnversionedDepSpec _ p' _ _ -> p'

                First mCtx <- ask

                let ctx = case mCtx of
                            Nothing -> Left (Nothing, s)
                            Just (Left dc) -> Left (Just dc, s)
                            Just (Right oc) -> Right (oc, s)

                pure $ DepVarMap
                     $ NEM.singleton p
                     $ CtxMap.singleton ctx

    finalize
        :: DepVar
        -> DepVarMap
        -> ConstraintMap
    finalize var (DepVarMap m0)
        = mconcat $ do
            let pwv = PkgWithVer p0 v0
            (pkg, cm) <- NEL.toList $ NEM.toList m0
            e <- CtxMap.toList cm >>= \case
                Left (ctx, DepMap nm) -> do
                    (spec, ()) <- NEL.toList $ NEM.toList nm
                    pure $ Left (ctx, spec)
                Right (ctx, OrGroupMap om) -> do
                    (spec, ()) <- NEL.toList $ NEM.toList om
                    pure $ Right (ctx, spec)
            pure $ force $ ConMap.singleton pkg pwv var e

    isBlocker :: DepSpec -> Bool
    isBlocker = \case
        VersionedDepSpec (Just _) _ _ _ -> True
        UnversionedDepSpec (Just _) _ _ _ -> True
        _ -> False

    foldMapM :: (Monad m, Foldable f, Monoid b) => (a -> m b) -> f a -> m b
    foldMapM f = Foldl.foldM (Foldl.sink f)


-- | Internal data structure for organizing a 'DepVar' entry
newtype DepVarMap = DepVarMap
    { getDepVarMap
        :: NEMMap Package (ContextMap 'Nothing)
    }
    deriving stock (Show)
    deriving newtype Semigroup
