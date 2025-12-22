
module RevdepScanner.Types.ConstraintMap
    ( ConstraintMap
    , insert
    , singleton
    ) where

import qualified Data.HashMap.Monoidal as HM
import           Data.HashMap.Monoidal (MonoidalHashMap)

import Distribution.Portage.Types

import RevdepScanner.Types
import qualified RevdepScanner.Types.ContextMap as CtxMap
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
