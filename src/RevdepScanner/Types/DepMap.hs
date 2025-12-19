{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}
{-# Language DerivingVia #-}
{-# Language LambdaCase #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module RevdepScanner.Types.DepMap
    ( DepMap(..)
    , OrGroupMap(..)
    , IsDepMap(..)
    , evalDepMap
    , liftMatchMode
    , AllDepMaps
    ) where

import Control.Monad
import Control.Monad.Writer
import Data.Kind
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import           Data.Map.Strict (Map)
import Data.Monoid

import Distribution.Portage.Types

import RevdepScanner.Types

-- | A normal dependency map. Each 'DepSpec' is tagged with an (optional)
--   bool wrapper (e.g. 'Any' or 'All'), depending on the 'MatchMode'.
newtype DepMap (m :: Maybe MatchMode) = DepMap
    { getDepMap :: NEMap DepSpec (MatchLogic DepMap m) }

deriving stock instance Show (MatchLogic DepMap m)
    => Show (DepMap m)
deriving stock instance Eq (MatchLogic DepMap m)
    => Eq (DepMap m)
deriving stock instance Ord (MatchLogic DepMap m)
    => Ord (DepMap m)
deriving newtype instance Ord (MatchLogic DepMap m)
    => Semigroup (DepMap m)

-- | Multiple 'DepSpec's with the same 'Package' found within an 'OrGroup'.
--   (This special case needs to be handled differently.) Each 'DepSpec' is
--   tagged with an (optional) bool wrapper (e.g. 'Any' or 'All'), depending on
--   the 'MatchMode'.
newtype OrGroupMap m = OrGroupMap
    { getOrGroupMap :: NEMap DepSpec (MatchLogic OrGroupMap m) }

deriving stock instance Show (MatchLogic OrGroupMap m)
    => Show (OrGroupMap m)
deriving stock instance Eq (MatchLogic OrGroupMap m)
    => Eq (OrGroupMap m)
deriving stock instance Ord (MatchLogic OrGroupMap m)
    => Ord (OrGroupMap m)
deriving newtype instance Ord (MatchLogic OrGroupMap m)
    => Semigroup (OrGroupMap m)

class   ( MatchLogic t 'Nothing ~ ()
        , IsBool (MatchLogic t ('Just 'Matching))
        , IsBool (MatchLogic t ('Just 'NonMatching)))
    => IsDepMap (t :: Maybe MatchMode -> Type) where
    -- | Encodes the logic for the different match modes at the type-level. This
    --   allows @'Nothing@ as an option for the times when the constraint map has
    --   been built but no matching logic has yet been applied.
    type MatchLogic t (m :: Maybe MatchMode) :: Type

    -- | Apply the newtype wrapper
    wrapDepMap :: NEMap DepSpec (MatchLogic t m) -> t m

    -- | Remove the 'NESet' from its newtype wrapper
    unwrapDepMap :: t m -> NEMap DepSpec (MatchLogic t m)

instance IsDepMap DepMap where
    type MatchLogic DepMap ('Just 'Matching) = All
    type MatchLogic DepMap ('Just 'NonMatching) = Any
    type MatchLogic DepMap 'Nothing = ()

    wrapDepMap = DepMap
    unwrapDepMap = getDepMap

instance IsDepMap OrGroupMap where
    type MatchLogic OrGroupMap ('Just 'Matching) = Any
    type MatchLogic OrGroupMap ('Just 'NonMatching) = All
    type MatchLogic OrGroupMap 'Nothing = ()

    wrapDepMap = OrGroupMap
    unwrapDepMap = getOrGroupMap

-- | Check if a dependency-set's elements are relevant, returning the
--   filtered results /if/ the whole dependency-set itself is deemed relevant.
--   This is evaluated using the 'MatchLogic' corresponding to the type of
--   dependency-set.
evalDepMap
    :: forall t m. (IsDepMap t, IsBool (MatchLogic t ('Just m)))
    => (DepSpec -> Bool) -- ^ A function to determine if a 'DepSpec' is relevant
    -> t 'Nothing -- ^ An unevaluated dependency-set
    -> Maybe (t ('Just m))
evalDepMap f ds
    = finish
    $ runWriter
    $ NEM.traverseMaybeWithKey1 go (unwrapDepMap ds)
  where
    go s () = let b = f s
                  l = fromBool b
              in do tell l -- Save the 'MatchLogic' value in the Writer monad
                    -- Filter out non-relevant entries
                    pure $ if b then Just l else Nothing

    finish
        :: ( Map DepSpec (MatchLogic t ('Just m))
            , MatchLogic t ('Just m))
        -> Maybe (t ('Just m))
    finish (m, l) = do
        -- Only return something if the whole depset is deemed relevant
        guard $ toBool l
        wrapDepMap <$> NEM.nonEmptyMap m

-- | Allows for working with type-level match modes when they can only be known
--   at runtime. (Pattern match on the 'LiftedMatchMode' passed to your function
--   to get a type-level witness of the current 'MatchMode').
liftMatchMode
    :: MatchMode
    -> ( forall (m :: MatchMode). AllDepMaps m '[ IsBool ]
            => LiftedMatchMode m -> r )
    -> r
liftMatchMode m f = case m of
    Matching -> f LMatching
    NonMatching -> f LNonMatching

-- | A "helper type family" that applies a given list of constraints to both
--   'DepMap' and 'OrGroupMap' (using the given 'MatchMode').
--
--   e.g. @AllDepMaps 'NonMatching '[ Ord, Monoid, Show ]@
type family AllDepMaps (m :: MatchMode) (cs :: [Type -> Constraint]) :: Constraint where
    AllDepMaps _ '[] = ()
    AllDepMaps m (c ': cs) =
        ( c (MatchLogic OrGroupMap ('Just m))
        , c (MatchLogic DepMap ('Just m))
        , AllDepMaps m cs )
