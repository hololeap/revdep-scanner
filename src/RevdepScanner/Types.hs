{-# Language DeriveAnyClass #-}
{-# Language DerivingVia #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module RevdepScanner.Types
    ( PkgWithVer(..)
    , DepWithCtx(..)
    , DepContext(..)
    , ResultMap
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Set.NonEmpty (NESet)
import GHC.Generics

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types
import Distribution.Portage.Types.Orphans ()

-- | A package and a version, which uniquely identifies an ebuild
data PkgWithVer = PkgWithVer
    { pwvPackage :: Package
    , pwvVersion :: Version
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Hashable

instance Printable PkgWithVer where
    toString (PkgWithVer p v) = toString p ++ "-" ++ toString v

instance Parsable PkgWithVer st String where
    parserName = "package with version (no constraint)"
    parser :: ParserT st String PkgWithVer
    parser = do
        p <- parser
        _ <- $( char '-' )
        v <- parser
        pure $ PkgWithVer p v

-- | A 'DepSpec' and it's context: the 'DepVar' where it was encountered and
--   its (optional) relevant 'DepContext'
data DepWithCtx = DepWithCtx
    { dwcDepSpec :: DepSpec
    , dwcDepVar :: DepVar
    , dwcDepContext :: Maybe DepContext
    } deriving (Show, Eq, Ord, Generic, Hashable)

-- | A relevant context within which a 'DepSpec' was found.
--
--   This can be:
--
--   [@'OrCtx'@]: When a relevant 'DepSpec' is encountered within a
--                @|| ( ... )@ block, it is useful to know the full
--                block where it was found. This is roughly equivalent to
--                'OrGroup'.
--   [@'UseCtx'@]: It was encountered within a @flag? ( ... )@ block. The
--                 context and USE flag are both stored. This is roughly
--                 equivalent to 'UseGroup'.
--   [@'NotUseCtx'@]: It is encountered within a @!flag? ( ... )@ block. The
--                    context and USE flag are both stored. This is roughly
--                    equivalent to 'NotUseGroup'.
--
--   Note that there is no context equivalent to 'AndGroup'; This is because
--   the normal @( ... )@ blocks are trivial and should not be needed in the
--   output for the user. This can be changed if needed.
data DepContext
    = OrCtx (NonEmpty (Either DepGroup DepSpec))
    | UseCtx (NonEmpty (Either DepGroup DepSpec)) UseFlag
    | NotUseCtx (NonEmpty (Either DepGroup DepSpec)) UseFlag
    deriving (Show, Eq, Ord, Generic, Hashable)

instance Printable DepContext where
    toString = \case
        OrCtx ne -> toString $ OrGroup ne
        UseCtx ne uf -> toString $ UseGroup ne uf
        NotUseCtx ne uf -> toString $ NotUseGroup ne uf

-- | A 'HashMap' from a package/version pair to relevant dependencies and
--   their context. This is produced by looking up a particular
--   package + version + mode in the main 'ContextMap'.
type ResultMap = Map PkgWithVer (NESet DepWithCtx)
