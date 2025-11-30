{-# Language DataKinds #-}
{-# Language DeriveAnyClass #-}
{-# Language DerivingVia #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module RevdepScanner.Types
    ( PkgWithVer(..)
    , CmdlinePkg(..)
    , cmdlinePkgPackage
    , MatchMode(..)
    , LiftedMatchMode(..)
    , DepContext(..)
    ) where

import Data.Hashable
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics

import Data.Parsable
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

-- | The user-supplied package (with optional version), given on the command-line
data CmdlinePkg
    = CmdlinePackage Package
    | CmdlinePkgWithVer PkgWithVer
    deriving (Show, Eq, Ord, Generic, Hashable)

instance Printable CmdlinePkg where
    toString (CmdlinePackage p) = toString p
    toString (CmdlinePkgWithVer pwv) = toString pwv

instance Parsable CmdlinePkg st String where
    parserName = "command-line given package with optional version"
    parser :: ParserT st String CmdlinePkg
    parser
        =   try eqParser
        <|> try (CmdlinePkgWithVer <$> parser)
        <|> (CmdlinePackage <$> parser)
      where
        -- | Allow for using @=app-misc/foo-0.1@ style
        eqParser :: ParserT st String CmdlinePkg
        eqParser = parser >>= \case
            VPkgEq p v -> pure $ CmdlinePkgWithVer (PkgWithVer p v)
            _ -> err "unsupported atom"

cmdlinePkgPackage :: CmdlinePkg -> Package
cmdlinePkgPackage = \case
    CmdlinePackage p -> p
    CmdlinePkgWithVer (PkgWithVer p _) -> p

-- | Controls the logic of the program
data MatchMode
    = Matching
    | NonMatching
    deriving (Show, Eq, Ord)

-- | Lifts into a type-level match mode from a data-level representation
data LiftedMatchMode :: MatchMode -> Type where
    LMatching :: LiftedMatchMode 'Matching
    LNonMatching :: LiftedMatchMode 'NonMatching

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
