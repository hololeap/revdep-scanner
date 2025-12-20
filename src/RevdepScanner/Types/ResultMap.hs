{-# Language DataKinds #-}
{-# Language LambdaCase #-}

module RevdepScanner.Types.ResultMap
    ( ResultMap'
    , UnevaluatedResultMap
    , EvaluatedResultMap
    , evalResultMap
    ) where

import Data.Kind
import Data.Map.Monoidal.Strict (MonoidalMap)

import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.ContextMap
import RevdepScanner.Types.DepMap
import qualified RevdepScanner.Types.NEMMap as NEM
import           RevdepScanner.Types.NEMMap (NEMMap)

-- | A 'Map' from a package\/version pair (e.g. the ebuild\/revdep) to
--   relevant dependencies and their context.
type ResultMap' (f :: Type -> Type -> Type) (m :: Maybe MatchMode)
    = f PkgWithVer -- revdep (ebuild)
        (NEMMap DepVar -- e.g. RDEPEND, DEPEND, etc
            (ContextMap m) -- relevant context
        )

type UnevaluatedResultMap = ResultMap' NEMMap 'Nothing
type EvaluatedResultMap m = ResultMap' MonoidalMap ('Just m)

-- | Evaluate and prune an entire tree of unevaluated dependencies,
--   removing any that are determined not to be relevant to the user's
--   query. This may return an empty 'Map' in the case that no dependencies
--   are relevant.
evalResultMap
    :: forall m. AllDepMaps m '[ IsBool ]
    => (DepSpec -> Bool) -- ^ A function to determine if a 'DepSpec' is relevant
    -> UnevaluatedResultMap -- ^ An unevaluated 'ResultMap''
    -> EvaluatedResultMap m
evalResultMap f
    = NEM.toMap . NEM.mapMaybe (NEM.mapMaybe (evalContextMap f))
