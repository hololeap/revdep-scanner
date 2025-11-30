{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}
{-# Language DerivingVia #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module RevdepScanner.Types.DepSet
    ( DepSet'(..)
    , DepSet
    , DepOrGroup'(..)
    , DepOrGroup
    , IsDepSet(..)
    , evalDepSet
    , liftMatchMode
    ) where

import Control.Monad (forM)
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid
import qualified Data.Set.NonEmpty as NES
import           Data.Set.NonEmpty (NESet)

import Distribution.Portage.Types

import RevdepScanner.Types

-- | A normal dependency set. Each 'DepSpec' is wrapped with a functor so that
--   they can be tagged later.
newtype DepSet' m = DepSet
    { getDepSet :: NESet (MatchLogic DepSet' m, DepSpec) }

deriving stock instance Show (MatchLogic DepSet' m)
    => Show (DepSet' m)
deriving stock instance Eq (MatchLogic DepSet' m)
    => Eq (DepSet' m)
deriving stock instance Ord (MatchLogic DepSet' m)
    => Ord (DepSet' m)
deriving newtype instance Ord (MatchLogic DepSet' m)
    => Semigroup (DepSet' m)

type DepSet = DepSet' 'Nothing

-- | Multiple 'DepSpec's with the same 'Package' found within an 'OrGroup'.
--   (This special case needs to be handled differently.) Each 'DepSpec' is
--   wrapped with a functor so that they can be tagged later.
newtype DepOrGroup' m = DepOrGroup
    { getDepOrGroup :: NESet (MatchLogic DepOrGroup' m, DepSpec) }

deriving stock instance Show (MatchLogic DepOrGroup' m)
    => Show (DepOrGroup' m)
deriving stock instance Eq (MatchLogic DepOrGroup' m)
    => Eq (DepOrGroup' m)
deriving stock instance Ord (MatchLogic DepOrGroup' m)
    => Ord (DepOrGroup' m)
deriving newtype instance Ord (MatchLogic DepOrGroup' m)
    => Semigroup (DepOrGroup' m)

type DepOrGroup = DepOrGroup' 'Nothing

class IsDepSet (t :: Maybe MatchMode -> Type) where
    -- | Encodes the logic for the different match modes at the type-level. This
    --   allows @'Nothing@ as an option for the times when the constraint map has
    --   been built but no matching logic has been applied.
    type MatchLogic t (m :: Maybe MatchMode) :: Type

    -- | Apply the newtype wrapper
    wrapDepSet :: NESet (MatchLogic t m, DepSpec) -> t m

    -- | Remove the 'NESet' from its newtype wrapper
    unwrapDepSet :: t m -> NESet (MatchLogic t m, DepSpec)

    -- | Lift a boolean to the correct 'MatchLogic'
    liftBool
        :: LiftedMatchMode m
        -> Bool
        -> MatchLogic t ('Just m)

    -- | Lower a 'MatchLogic' value to 'Bool'
    lowerBool
        :: LiftedMatchMode m
        -> MatchLogic t ('Just m)
        -> Bool

instance IsDepSet DepSet' where
    type MatchLogic DepSet' ('Just 'Matching) = All
    type MatchLogic DepSet' ('Just 'NonMatching) = Any
    type MatchLogic DepSet' 'Nothing = ()

    wrapDepSet = DepSet
    unwrapDepSet = getDepSet

    liftBool mode b = case mode of
        LMatching -> All b
        LNonMatching -> Any b

    lowerBool mode l = case (mode, l) of
        (LMatching, All b) -> b
        (LNonMatching, Any b) -> b

instance IsDepSet DepOrGroup' where
    type MatchLogic DepOrGroup' ('Just 'Matching) = Any
    type MatchLogic DepOrGroup' ('Just 'NonMatching) = All
    type MatchLogic DepOrGroup' 'Nothing = ()

    wrapDepSet = DepOrGroup
    unwrapDepSet = getDepOrGroup

    liftBool mode b = case mode of
        LMatching -> Any b
        LNonMatching -> All b

    lowerBool mode l = case (mode, l) of
        (LMatching, Any b) -> b
        (LNonMatching, All b) -> b

-- | Check if a dependency-set's elements are relevant, returning the
--   cached results along with a "summary" @Bool@. This @Bool@ signifies
--   if the dependency-set itself is relevant, which is evaluated using the
--   'MatchLogic' corresponding to the type of dependency-set.
evalDepSet
    :: forall t m.
        ( IsDepSet t
        , MatchLogic t 'Nothing ~ ()
        , Monoid (MatchLogic t ('Just m))
        , Ord (MatchLogic t ('Just m))
    ) => LiftedMatchMode m -- ^ The 'MatchMode' given on the command line
    -> (DepSpec -> Bool) -- ^ A function to determine if a 'DepSpec' is relevant
    -> t 'Nothing -- ^ An unevaluated dependency-set
    -> (Bool, t ('Just m))
evalDepSet mode f ds
    = uncurry finish
    $ forM (NES.toList (unwrapDepSet ds))
    $ \((), s) ->
        let l = liftBool @t mode (f s)
        in (l, (l, s))
  where
    finish
        :: MatchLogic t ('Just m)
        -> NonEmpty (MatchLogic t ('Just m), DepSpec)
        -> (Bool, t ('Just m))
    finish l ne = (lowerBool @t mode l, wrapDepSet (NES.fromList ne))

-- | Allows for working with type-level match modes when they can only be known
--   at runtime. (Pattern match on the 'LiftedMatchMode' passed to your function
--   to get a type-level witness of the current 'MatchMode').
liftMatchMode
    :: MatchMode
    -> ( forall (m :: MatchMode).
               ( Monoid (MatchLogic DepOrGroup' ('Just m))
               , Monoid (MatchLogic DepSet' ('Just m))
               , Ord (MatchLogic DepOrGroup' ('Just m))
               , Ord (MatchLogic DepSet' ('Just m))
               , Show (MatchLogic DepOrGroup' ('Just m))
               , Show (MatchLogic DepSet' ('Just m))
               )
            => LiftedMatchMode m -> r )
    -> r
liftMatchMode m f = case m of
    Matching -> f LMatching
    NonMatching -> f LNonMatching
