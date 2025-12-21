{-# Language DataKinds #-}
{-# Language LambdaCase #-}

module RevdepScanner.Types.ConstraintMap
    ( ConstraintMap
    , insert
    , singleton
    , buildCMap
    ) where

import Control.DeepSeq
import Control.Monad.Trans.Accum
import qualified Data.HashMap.Monoidal as HM
import           Data.HashMap.Monoidal (MonoidalHashMap)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Monoidal.Strict as M
import           Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Monoid

import Distribution.Portage.Types
import Distribution.Gentoo.Utils.Pquery

import RevdepScanner.Types
import qualified RevdepScanner.Types.ContextMap as CtxMap
import           RevdepScanner.Types.ContextMap (ContextMap)
import RevdepScanner.Types.DepMap
import qualified RevdepScanner.Types.NEMMap as NEM
import RevdepScanner.Types.ResultMap

-- | Organized by @'Package'@ (@(Category, PkgName)@)
--
--   The inner map is keyed by the @'PkgWithVer'@ and contains a set of
--   @'DepWithCtx'@ that match the same @Package@ as the outermost key.
type ConstraintMap = MonoidalHashMap Package UnevaluatedResultMap

insert
    :: Package
    -> PkgWithVer
    -> DepVar
    -> Either
            (Maybe DepContext, DepSpec)
            (OrContext, DepSpec)
    -> ConstraintMap
    -> ConstraintMap
insert pkg pwv dVar ctx
    = (singleton pkg pwv dVar ctx <>)

singleton
    :: Package
    -> PkgWithVer
    -> DepVar
    -> Either
            (Maybe DepContext, DepSpec)
            (OrContext, DepSpec)
    -> ConstraintMap
singleton pkg pwv dVar ctx
    = HM.singleton pkg
    $ NEM.singleton pwv
    $ NEM.singleton dVar
    $ CtxMap.singleton ctx

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
        -> Accum (First (Either DepContext OrContext)) (Maybe DepVarMap)
    fromGroup = \case
        Left g -> do
            case g of
                -- "And" groups are very basic. We don't need to remember them
                AndGroup ne -> foldMapA fromGroup ne
                -- The other groups are more complex and should be saved as
                -- context for the output
                OrGroup ne -> do
                    add $ pure $ Right $ OrCtx ne
                    foldMapA fromGroup ne
                UseGroup ne u -> do
                    add $ pure $ Left $ UseCtx ne u
                    foldMapA fromGroup ne
                NotUseGroup ne u -> do
                    add $ pure $ Left $ NotUseCtx ne u
                    foldMapA fromGroup ne
        Right s
            | isBlocker s -> pure Nothing -- Skip blockers
            | otherwise -> Just <$> do
                let p = case s of
                        VersionedDepSpec _ vp _ _ -> vPkgPackage vp
                        UnversionedDepSpec _ p' _ _ -> p'

                First mCtx <- look

                let ctx = case mCtx of
                            Nothing -> Left (Nothing, s)
                            Just (Left dc) -> Left (Just dc, s)
                            Just (Right oc) -> Right (oc, s)

                pure $ DepVarMap
                     $ M.singleton p
                     $ CtxMap.singleton ctx

    finalize
        :: DepVar
        -> DepVarMap
        -> ConstraintMap
    finalize var (DepVarMap m0)
        = mconcat $ do
            let pwv = PkgWithVer p0 v0
            (pkg, cm) <- M.toList m0
            e <- CtxMap.toList cm >>= \case
                Left (ctx, DepMap nm) -> do
                    (spec, ()) <- NEL.toList $ NEM.toList nm
                    pure $ Left (ctx, spec)
                Right (ctx, OrGroupMap om) -> do
                    (spec, ()) <- NEL.toList $ NEM.toList om
                    pure $ Right (ctx, spec)
            pure $ force $ singleton pkg pwv var e

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
        :: MonoidalMap Package (ContextMap 'Nothing)
    } deriving (Show)

instance Semigroup DepVarMap where
    DepVarMap m1 <> DepVarMap m2 = DepVarMap $ M.unionWith (<>) m1 m2

instance Monoid DepVarMap where
    mempty = DepVarMap M.empty
