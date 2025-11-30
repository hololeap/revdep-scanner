{-# Language DataKinds #-}
{-# Language LambdaCase #-}

module RevdepScanner.Types.ConstraintMap
    ( ConstraintMap
    , insert
    , union
    , unions
    , singleton
    , buildCMap
    ) where

import Control.Monad.Trans.Accum
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.Monoid
import qualified Data.Set.NonEmpty as NES
import           Data.Set.NonEmpty (NESet)

import Distribution.Portage.Types
import Distribution.Gentoo.Utils.Pquery

import RevdepScanner.Types
import RevdepScanner.Types.DepSet
import RevdepScanner.Types.ResultMap

-- | Organized by @'Package'@ (@(Category, PkgName)@)
--
--   The inner map is keyed by the @'PkgWithVer'@ and contains a set of
--   @'DepWithCtx'@ that match the same @Package@ as the outermost key.
type ConstraintMap = HashMap Package UnevaluatedResultMap

insert
    :: Package -> PkgWithVer
    -> DepVar -> Maybe DepContext -> (Either DepOrGroup DepSet)
    -> ConstraintMap -> ConstraintMap
insert pkg pwv dVar dCtx depSet
    = (singleton pkg pwv dVar dCtx depSet `union`)

union :: ConstraintMap -> ConstraintMap -> ConstraintMap
union
    = HM.unionWith
    $ NEM.unionWith
    $ NEM.unionWith
    $ NEM.unionWith (<>)

unions :: [ConstraintMap] -> ConstraintMap
unions = foldl' union HM.empty

singleton
    :: Package -> PkgWithVer
    -> DepVar -> Maybe DepContext -> (Either DepOrGroup DepSet)
    -> ConstraintMap
singleton pkg pwv dVar mCtx depSet
    = HM.singleton pkg
    $ NEM.singleton pwv
    $ NEM.singleton dVar
    $ NEM.singleton mCtx
    $ depSet

-- | Build a 'ConstraintMap' by scanning the contents of a 'PkgDeps' entry
buildCMap :: PkgDeps -> ConstraintMap
-- TODO: Should pquery/PkgDeps ignore the slot value completely?
buildCMap (PkgDeps (p0,v0,_) depBlock rdepBlock bdepBlock pdepBlock idepBlock) =
    -- Needs to find all packages referenced by any of the blocks, note
    -- which package the dep block lives in, and its name.
    flip evalAccum mempty $ flip foldMapA
        [ (DEPEND, depBlock)
        , (RDEPEND, rdepBlock)
        , (BDEPEND, bdepBlock)
        , (PDEPEND, pdepBlock)
        , (IDEPEND, idepBlock)
        ] $ \(var, DepBlock blk) ->
            foldMap (finalize var) <$> foldMapA fromGroup blk
  where
    fromGroup
        :: Either DepGroup DepSpec
        -> Accum (First DepContext) (Maybe DepVarMap)
    fromGroup = \case
        Left g -> do
            case g of
                -- "And" groups are very basic. We don't need to remember them
                AndGroup ne -> foldMapA fromGroup ne
                -- The other groups are more complex and should be saved as
                -- context for the output
                OrGroup ne -> do
                    add $ pure $ OrCtx ne
                    foldMapA fromGroup ne
                UseGroup ne u -> do
                    add $ pure $ UseCtx ne u
                    foldMapA fromGroup ne
                NotUseGroup ne u -> do
                    add $ pure $ NotUseCtx ne u
                    foldMapA fromGroup ne
        Right s
            | isBlocker s -> pure Nothing -- Skip blockers
            | otherwise -> Just <$> do
                let p = case s of
                        VersionedDepSpec _ vp _ _ -> vPkgPackage vp
                        UnversionedDepSpec _ p' _ _ -> p'

                First mCtx <- look

                pure $ DepVarMap
                     $ M.singleton p
                     $ NEM.singleton mCtx
                     $ NES.singleton s

    finalize
        :: DepVar
        -> DepVarMap
        -> ConstraintMap
    finalize var (DepVarMap m0)
        = unions $ do
            (pkg, m1) <- M.toList m0
            (mCtx, dss) <- NEL.toList $ NEM.toList m1
            pure $ singleton pkg (PkgWithVer p0 v0) var mCtx $
                case mCtx of
                    Just (OrCtx _) -> Left (DepOrGroup (NES.map pure dss))
                    _ -> Right (DepSet (NES.map pure dss))

    isBlocker :: DepSpec -> Bool
    isBlocker = \case
        VersionedDepSpec (Just _) _ _ _ -> True
        UnversionedDepSpec (Just _) _ _ _ -> True
        _ -> False

    foldMapA :: (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
    foldMapA f = getAp . foldMap (Ap . f)

-- | Internal data structure for organizing a 'DepVar' entry
newtype DepVarMap = DepVarMap
    { getDepVarMap
        :: Map Package (NEMap (Maybe DepContext) (NESet DepSpec))
    } deriving (Show, Eq, Ord)

instance Semigroup DepVarMap where
    DepVarMap m1 <> DepVarMap m2 = DepVarMap $ M.unionWith (NEM.unionWith NES.union) m1 m2

instance Monoid DepVarMap where
    mempty = DepVarMap M.empty
