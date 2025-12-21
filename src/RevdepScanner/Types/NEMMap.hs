{-|
Module      : RevdepScanner.Types.NEMMap

Non-empty monoidal maps
-}

{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language DeriveTraversable #-}
{-# Language GeneralizedNewtypeDeriving #-}

module RevdepScanner.Types.NEMMap
    ( NEMMap
    , toList
    , fromList
    , keys
    , traverseMaybeWithKey1
    , nonEmptyMap
    , singleton
    , mapMaybe
    , toMap
    ) where

import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import Data.Functor.Apply
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Monoidal.Strict as MM
import           Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import           Data.Map.Strict (Map)
import Data.Foldable1
import Data.Semigroup.Traversable
import GHC.Generics (Generic)

newtype NEMMap k a = NEMMap { getNEMMap :: NEMap k a}
    deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
    deriving anyclass NFData

instance (Ord k, Semigroup a) => Semigroup (NEMMap k a) where
    NEMMap m1 <> NEMMap m2 = NEMMap $ NEM.unionWith (<>) m1 m2

instance Foldable1 (NEMMap k) where
    foldMap1 f = foldMap1 f . getNEMMap
    foldrMap1 f g = foldrMap1 f g . getNEMMap

instance Traversable1 (NEMMap k) where
    traverse1 f = fmap NEMMap . traverse1 f . getNEMMap
    sequence1 = fmap NEMMap . sequence1 . getNEMMap


toList :: NEMMap k a -> NonEmpty (k,a)
toList = NEM.toList . getNEMMap

fromList :: Ord k => NonEmpty (k,a) -> NEMMap k a
fromList = NEMMap . NEM.fromList

keys :: NEMMap k a -> NonEmpty k
keys = NEM.keys . getNEMMap

traverseMaybeWithKey1
    :: Apply t
    => (k -> a -> t (Maybe b))
    -> NEMMap k a
    -> t (Maybe (NEMMap k b))
traverseMaybeWithKey1 f = fmap nonEmptyMap . NEM.traverseMaybeWithKey1 f . getNEMMap

nonEmptyMap :: Map k a -> Maybe (NEMMap k a)
nonEmptyMap = fmap NEMMap . NEM.nonEmptyMap

singleton :: k -> a -> NEMMap k a
singleton k = NEMMap . NEM.singleton k

mapMaybe
    :: (a -> Maybe b)
    -> NEMMap k a
    -> Maybe (NEMMap k b)
mapMaybe f = nonEmptyMap . NEM.mapMaybe f . getNEMMap

-- | Transform a 'NEMMap' to a 'MonoidalMap'. Creates an empty 'MonoidalMap'
--   if given @Nothing@.
toMap :: (Ord k, Semigroup a) => Maybe (NEMMap k a) -> MonoidalMap k a
toMap m = fold $ MM.MonoidalMap . NEM.toMap . getNEMMap <$> m
