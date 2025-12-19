{-# Language DataKinds #-}
{-# Language LambdaCase #-}

module RevdepScanner.Types.ResultMap
    ( ResultMap'
    , UnevaluatedResultMap
    , EvaluatedResultMap
    , evalResultMap
    , unionUnevaluatedResultMaps
    , unionEvaluatedResultMaps
    ) where

import Data.Kind
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.ContextMap
import RevdepScanner.Types.DepMap

-- | A 'Map' from a package/version pair (e.g. the ebuild/revdep) to
--   relevant dependencies and their context.
type ResultMap' (f :: Type -> Type -> Type) (m :: Maybe MatchMode)
    = f PkgWithVer -- revdep (ebuild)
        (NEMap DepVar -- e.g. RDEPEND, DEPEND, etc
            (ContextMap m) -- relevant context
        )

type UnevaluatedResultMap = ResultMap' NEMap 'Nothing
type EvaluatedResultMap m = ResultMap' Map ('Just m)

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
    = NEM.mapMaybe
    $ NEM.nonEmptyMap
    . NEM.mapMaybe (evalContextMap f)

unionUnevaluatedResultMaps
    :: UnevaluatedResultMap
    -> UnevaluatedResultMap
    -> UnevaluatedResultMap
unionUnevaluatedResultMaps
    = NEM.unionWith
    $ NEM.unionWith (<>)

unionEvaluatedResultMaps
    :: AllDepMaps m '[ IsBool ]
    => EvaluatedResultMap m
    -> EvaluatedResultMap m
    -> EvaluatedResultMap m
unionEvaluatedResultMaps
    = M.unionWith
    $ NEM.unionWith (<>)
