{-# Language DataKinds #-}
{-# Language LambdaCase #-}

module RevdepScanner.Types.ResultMap
    ( ResultMap'
    , UnevaluatedResultMap
    , EvaluatedResultMap
    , evalResultMap
    , foldResultMap
    ) where

import Data.Foldable (foldl')
import Data.Kind
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.DepSet

-- | A 'Map' from a package/version pair (e.g. the ebuild/revdep) to
--   relevant dependencies and their context.
type ResultMap' (f :: Type -> Type -> Type) (m :: Maybe MatchMode)
    = f PkgWithVer -- revdep (ebuild)
        (NEMap DepVar -- e.g. RDEPEND, DEPEND, etc
            ((NEMap (Maybe DepContext) -- relevant context
                -- normal dependency, or an 'OrGroup'
                ((Either (DepOrGroup' m) (DepSet' m)))
            ))
        )

type UnevaluatedResultMap = ResultMap' NEMap 'Nothing
type EvaluatedResultMap m = ResultMap' Map ('Just m)

-- | Evaluate and prune an entire tree of unevaluated dependencies,
--   removing any that are determined not to be relevant to the user's
--   query. This may return an empty 'Map' in the case that no dependencies
--   are relevant.
evalResultMap
    :: forall m.
        ( Monoid (MatchLogic DepSet' ('Just m))
        , Monoid (MatchLogic DepOrGroup' ('Just m))
        , Ord (MatchLogic DepSet' ('Just m))
        , Ord (MatchLogic DepOrGroup' ('Just m))
    ) => LiftedMatchMode m -- ^ The 'MatchMode' given on the command line
    -> (DepSpec -> Bool) -- ^ A function to determine if a 'DepSpec' is relevant
    -> UnevaluatedResultMap -- ^ An unevaluated 'ResultMap''
    -> EvaluatedResultMap m
evalResultMap mode f
    = NEM.mapMaybe
    $ (NEM.nonEmptyMap .) $ NEM.mapMaybe
    $ (NEM.nonEmptyMap .) $ NEM.mapMaybe
    $ \case
        Left ds -> case evalDepSet mode f ds of
            (False, _) -> Nothing
            (True, ds') -> Just $ Left ds'
        Right ds -> case evalDepSet mode f ds of
            (False, _) -> Nothing
            (True, ds') -> Just $ Right ds'

-- | Strict fold of an 'EvaluatedResultMap'
foldResultMap
    ::( PkgWithVer
            -> DepVar
            -> Maybe DepContext
            -> Either (DepOrGroup' ('Just m)) (DepSet' ('Just m))
            -> b
            -> b )
    -> b
    -> EvaluatedResultMap m
    -> b
foldResultMap f z m0 = foldl' go z $ do
    (pwv, m1) <- M.toList m0
    (dv, m2) <- NEL.toList (NEM.toList m1)
    (mdc, ds) <- NEL.toList (NEM.toList m2)
    pure (pwv, dv, mdc, ds)
  where
    go x (pwv, dv, mdc, ds) = f pwv dv mdc ds x
