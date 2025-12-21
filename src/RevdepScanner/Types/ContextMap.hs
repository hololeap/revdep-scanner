{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language LambdaCase #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module RevdepScanner.Types.ContextMap
    ( ContextMap(..)
    , evalContextMap
    , singleton
    , toList
    ) where

import Control.DeepSeq (NFData)
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup.Traversable
import GHC.Generics (Generic)

import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.DepMap
import qualified RevdepScanner.Types.NEMMap as NEM
import           RevdepScanner.Types.NEMMap (NEMMap)

-- | A map from an optional specific context to its dependency map. This can
--   hold a normal @NEMap (Maybe DepContext) (DepMap m)@, a variant for
--   @|| ( )@ blocks, or a mixture of both. A 'ContextMap' will always contain
--   at least one element.
data ContextMap (m :: Maybe MatchMode) where
    MixedCtxMap
        :: NEMMap (Maybe DepContext) (DepMap m)
        -> NEMMap OrContext (OrGroupMap m)
        -> ContextMap m
    NormalCtxMap :: NEMMap (Maybe DepContext) (DepMap m) -> ContextMap m
    OrGroupCtxMap :: NEMMap OrContext (OrGroupMap m) -> ContextMap m
    deriving stock Generic

deriving instance
        ( Show (MatchLogic DepMap m)
        , Show (MatchLogic OrGroupMap m) )
   => Show (ContextMap m)

deriving instance
        ( Eq (MatchLogic DepMap m)
        , Eq (MatchLogic OrGroupMap m) )
   => Eq (ContextMap m)

deriving anyclass instance
        ( NFData (MatchLogic DepMap m)
        , NFData (MatchLogic OrGroupMap m) )
    => NFData (ContextMap m)

instance ( Ord (MatchLogic DepMap m)
         , Ord (MatchLogic OrGroupMap m)
         , Semigroup (MatchLogic DepMap m)
         , Semigroup (MatchLogic OrGroupMap m))
         => Semigroup (ContextMap m) where
    MixedCtxMap nm1 om1 <> MixedCtxMap nm2 om2
        = MixedCtxMap (nm1 <> nm2) (om1 <> om2)
    MixedCtxMap nm1 om1 <> NormalCtxMap nm2
        = MixedCtxMap (nm1 <> nm2) om1
    MixedCtxMap nm1 om1 <> OrGroupCtxMap om2
        = MixedCtxMap nm1 (om1 <> om2)
    NormalCtxMap nm1 <> MixedCtxMap nm2 om2
        = MixedCtxMap (nm1 <> nm2) om2
    NormalCtxMap nm1 <> NormalCtxMap nm2
        = NormalCtxMap (nm1 <> nm2)
    NormalCtxMap nm1 <> OrGroupCtxMap om2
        = MixedCtxMap nm1 om2
    OrGroupCtxMap om1 <> MixedCtxMap nm2 om2
        = MixedCtxMap nm2 (om1 <> om2)
    OrGroupCtxMap om1 <> NormalCtxMap nm2
        = MixedCtxMap nm2 om1
    OrGroupCtxMap om1 <> OrGroupCtxMap om2
        = OrGroupCtxMap (om1 <> om2)

-- | Evaluate all the dependency maps inside a 'ContextMap', removing any that
--   are deemed non-relevant. This will return @Nothing@ in the case that no
--   relevant dependency maps remain within the 'ContextMap'.
evalContextMap
    :: forall m. AllDepMaps m '[ IsBool ]
    => (DepSpec -> Bool)
    -> ContextMap 'Nothing
    -> Maybe (ContextMap ('Just m))
evalContextMap f = \case
    MixedCtxMap nm0 om0 ->
        case (traverse1 (evalDepMap f) nm0, traverse1 (evalDepMap f) om0) of
            (Just nm, Just om) -> Just $ MixedCtxMap nm om
            (Just nm, Nothing) -> Just $ NormalCtxMap nm
            (Nothing, Just om) -> Just $ OrGroupCtxMap om
            (Nothing, Nothing) -> Nothing
    NormalCtxMap nm ->
          fmap NormalCtxMap
        $ traverse1 (evalDepMap f) nm
    OrGroupCtxMap om ->
          fmap OrGroupCtxMap
        $ traverse1 (evalDepMap f) om

singleton
    :: Either
        (Maybe DepContext, DepSpec)
        (OrContext, DepSpec)
    -> ContextMap 'Nothing
singleton = \case
    Left (c, s) -> NormalCtxMap $ NEM.singleton c $ DepMap $ NEM.singleton s ()
    Right (c, s) -> OrGroupCtxMap $ NEM.singleton c $ OrGroupMap $ NEM.singleton s ()

toList
    :: ContextMap m
    -> [Either (Maybe DepContext, DepMap m) (OrContext, OrGroupMap m)]
toList = \case
    MixedCtxMap nm om
        -> (Left <$> NEL.toList (NEM.toList nm))
        ++ (Right <$> NEL.toList (NEM.toList om))
    NormalCtxMap nm -> Left <$> NEL.toList (NEM.toList nm)
    OrGroupCtxMap om -> Right <$> NEL.toList (NEM.toList om)
