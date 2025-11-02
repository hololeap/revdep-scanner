{-# Language LambdaCase #-}

module RevdepScanner.Types.ConstraintMap
    ( ConstraintMap
    , insert
    , union
    , buildCMap
    ) where

import Control.Monad
import Control.Monad.Trans.Accum
import qualified Data.HashSet as S
import           Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import Data.Monoid

import Distribution.Portage.Types
import Distribution.Gentoo.Utils.Pquery

import RevdepScanner.Types

-- | Organized by @'Package'@ (@(Category, PkgName)@)
--
--   The inner map is keyed by the @'PkgWithVer'@ and contains a set of
--   @'DepWithCtx'@ that match the same @Package@ as the outermost key.
type ConstraintMap = HashMap Package
    (HashMap PkgWithVer (HashSet DepWithCtx))

insert
    :: Package -> PkgWithVer
    -> DepVar -> DepSpec -> Maybe DepContext
    -> ConstraintMap -> ConstraintMap
insert pkg pwv dVar dSpec dCtx
    = union
    $ M.singleton pkg
    $ M.singleton pwv
    $ S.singleton (DepWithCtx dSpec dVar dCtx)

union :: ConstraintMap -> ConstraintMap -> ConstraintMap
union = M.unionWith (M.unionWith S.union)

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
            foldM groupFold M.empty blk `runReaderT` var
  where
    fromGroup
        :: Either DepGroup DepSpec
        -> ReaderT DepVar (Accum (First DepContext)) ConstraintMap
    fromGroup = \case
        Left g -> case g of
            -- "And" groups are very basic. We don't need to remember them
            AndGroup ne -> foldM groupFold M.empty ne
            -- The other groups are more complex and should be saved as
            -- context for the output
            OrGroup ne -> do
                lift $ add $ pure $ OrCtx ne
                foldM groupFold M.empty ne
            UseGroup ne u -> do
                lift $ add $ pure $ UseCtx ne u
                foldM groupFold M.empty ne
            NotUseGroup ne u -> do
                lift $ add $ pure $ NotUseCtx ne u
                foldM groupFold M.empty ne
        Right s -> do
            var <- ask
            First mCtx <- lift look
            -- Extract the package from the entry's DepSpec
            let p = case s of
                        VersionedDepSpec _ p' _ _ -> vPkgPackage p'
                        UnversionedDepSpec _ p' _ _ -> p'
            pure $ insert p (PkgWithVer p0 v0) var s mCtx M.empty

    groupFold
        :: ConstraintMap
        -> Either DepGroup DepSpec
        -> ReaderT DepVar (Accum (First DepContext)) ConstraintMap
    groupFold m g = M.unionWith (M.unionWith S.union) m <$> fromGroup g

    foldMapA :: (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
    foldMapA f = getAp . foldMap (Ap . f)
