{-# Language ApplicativeDo #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}

module Main (main) where

import Conduit
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Accum
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.Function (on)
import Data.Hashable
import Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as S
import           Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid
import GHC.Generics
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO (stderr, hPutStrLn)

import Text.Pretty.Simple (pPrintForceColor)

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types
import Distribution.Portage.Types.Orphans ()
import Distribution.Gentoo.Utils.Exe
import Distribution.Gentoo.Utils.Pquery

main :: IO ()
main = do
    (ps, mode, repo, Any d) <- checkArgs

    vDeps <- runExeEnv $ do
        when d $ liftIO $ print $ unwords $ "pquery" : args repo
        getPqueryDump ["--repo", unwrapRepository repo] buildCMap

    case vDeps of
        Failure es -> error $ "Parsing failure: " ++ show es
        Success deps -> do
            let (m :: ConstraintMap) = foldl' unionCM M.empty deps

            when d $ pPrintForceColor deps

            let ls = ps >>= \ep -> do
                    let p = case ep of
                            Left p' -> p'
                            Right (PkgWithVer p' _) -> p'
                        r = lookupResults mode ep m
                    case mode of
                        Matching -> prettyMatches p r
                        NonMatching -> prettyProblems p r
            putStr $ unlines $ NE.toList ls
  where
    args (Repository n) =
        [ "--all"
        , "--raw"
        , "--unfiltered"
        , "--repo", n
        , "--atom"
        , "--cpv"
        , "--slot"
        , "--attr", "depend"
        , "--attr", "rdepend"
        , "--attr", "bdepend"
        , "-R"
        , "--slot"
        ]

-- | Build a 'ConstraintMap' by scanning the contents of a 'PkgDeps' entry
buildCMap :: PkgDeps -> ConstraintMap
-- TODO: Should pquery/PkgDeps ignore the slot value completely?
buildCMap (PkgDeps (p0,v0,_) depBlock rdepBlock bdepBlock pdepBlock idepBlock) =
    -- Needs to find all packages referenced by any of the blocks, note
    -- which package the dep block lives in, and its name.
    flip evalAccum mempty $ flip foldMapA
        [ (DEPEND, depBlock)
        , (RDEPEND, rdepBlock)
        , (BDEPEND, bdepBlock)
        , (PDEPEND, pdepBlock)
        , (IDEPEND, idepBlock)
        ] $ \(var, DepBlock blk) ->
            foldM groupFold M.empty blk `runReaderT` var
  where
    fromGroup
        :: Either DepGroup DepSpec
        -> ReaderT DepVar (Accum (First DepContext)) ConstraintMap
    fromGroup = \case
        Left g -> case g of
            -- "And" groups are very basic. We don't need to remember them
            AndGroup ne -> foldM groupFold M.empty ne
            -- The other groups are more complex and should be saved as
            -- context for the output
            OrGroup ne -> do
                lift $ add $ pure $ AnyCtx ne
                foldM groupFold M.empty ne
            UseGroup ne u -> do
                lift $ add $ pure $ UseCtx ne u
                foldM groupFold M.empty ne
            NotUseGroup ne u -> do
                lift $ add $ pure $ NotUseCtx ne u
                foldM groupFold M.empty ne
        Right s -> do
            var <- ask
            First mCtx <- lift look
            -- Extract the package from the entry's DepSpec
            let p = case s of
                        VersionedDepSpec _ p' _ _ -> vPkgPackage p'
                        UnversionedDepSpec _ p' _ _ -> p'
            pure $ insertCM p (PkgWithVer p0 v0) var s mCtx M.empty

    groupFold
        :: ConstraintMap
        -> Either DepGroup DepSpec
        -> ReaderT DepVar (Accum (First DepContext)) ConstraintMap
    groupFold m g = M.unionWith (M.unionWith S.union) m <$> fromGroup g

    foldMapA :: (Applicative f, Foldable t, Monoid b) => (a -> f b) -> t a -> f b
    foldMapA f = getAp . foldMap (Ap . f)

prettyProblems
    :: Package
    -> ResultMap
    -> NonEmpty String
prettyProblems p m
    | M.null m = NE.singleton
        $ toString p ++ ": No problematic packages found!"
    | otherwise
        = (toString p ++ ":")
        :| prettyResults m

prettyMatches
    :: Package
    -> ResultMap
    -> NonEmpty String
prettyMatches p m
    | M.null m = NE.singleton
        $ toString p ++ ": No matches found!"
    | otherwise
        = (toString p ++ ":")
        :| prettyResults m

prettyResults :: ResultMap -> [String]
prettyResults m =
    sortBy (compare `on` fst) (M.toList m) >>= \((PkgWithVer p v),s) ->
        let p' = VPkgEq p v
            svs = sortBy cmp (S.toList s)
        in  [ "    " ++ toString p'
            , "        ( " ++ L.intercalate " " (map toStr svs) ++ " )"
            ]
  where
    cmp :: DepWithCtx -> DepWithCtx -> Ordering
    cmp dwc1 dwc2 = case (dwcDepSpec dwc1, dwcDepSpec dwc2) of
        (VersionedDepSpec _ vpkg1 _ _, VersionedDepSpec _ vpkg2 _ _)
            -> vpkg1 `compare` vpkg2
        (VersionedDepSpec _ _ _ _, UnversionedDepSpec _ _ _ _) -> GT
        (UnversionedDepSpec _ _ _ _, VersionedDepSpec _ _ _ _) -> LT
        (_, _) -> EQ

    toStr :: DepWithCtx -> String
    toStr = \case
        DepWithCtx _ dv (Just ctx) -> toString dv ++ ": " ++ toString ctx
        DepWithCtx ds dv Nothing -> toString dv ++ ": " ++ toString ds

-- | Given the 'MatchMode' and a package given on the command line (either
--   'Package' or 'PkgWithVer'), look up a mapping of reverse dependencies
--   to dependencies in the 'ConstraintMap'.
--
--   For instance, looking up @dev-haskell/cabal@ would produce a mapping:
--
--   @@@
--   (revdep of dev-haskell/cabal e.g. 'PkgWithVer')
--       -> ( relevant dependencies containing dev-haskell/cabal
--            e.g. (HashSet ('DepVar', 'DepSpec', Maybe 'DepContext'))
--          )
--   @@@
lookupResults
    :: MatchMode
    -> Either Package PkgWithVer
    -> ConstraintMap
    -> ResultMap
lookupResults mode ep =
    foldMap (M.filter (any check)) . M.lookup (either id pwvPackage ep)
  where
    check :: DepWithCtx -> Bool
    check = isDepRelevant mode ep . dwcDepSpec

-- | Check if a 'DepSpec' should be displayed, given the 'MatchMode' and
--   package (with optional version) from the command line.
isDepRelevant
    :: MatchMode
    -> Either Package PkgWithVer
    -> DepSpec
    -> Bool
isDepRelevant m0 e0 s0 =
    let b = case (e0, s0) of
            -- Ignore the DepSpec if it's a blocker
            (_, VersionedDepSpec (Just _) _ _ _) -> False
            (_, UnversionedDepSpec (Just _) _ _ _) -> False

            (Left p0, VersionedDepSpec _ vp _ _)
                -> p0 == vPkgPackage vp
            (Left p0, UnversionedDepSpec _ p _ _)
                -> p0 == p
            (Right (PkgWithVer p0 v0), VersionedDepSpec _ vp _ _)
                -> matchVersionedPackage vp p0 v0
            (Right (PkgWithVer p0 _), UnversionedDepSpec _ p _ _)
                -> p0 == p
    in case m0 of
            Matching -> b
            NonMatching -> not b

-- Types

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

-- | Organized by @'Package'@ (@(Category, PkgName)@)
--
--   The inner map is keyed by the @'PkgWithVer'@ and contains a set of
--   @'DepWithCtx'@ that match the same @Package@ as the outermost key.
type ConstraintMap = HashMap Package
    (HashMap PkgWithVer (HashSet DepWithCtx))

insertCM
    :: Package -> PkgWithVer
    -> DepVar -> DepSpec -> Maybe DepContext
    -> ConstraintMap -> ConstraintMap
insertCM pkg pwv dVar dSpec dCtx
    = unionCM
    $ M.singleton pkg
    $ M.singleton pwv
    $ S.singleton (DepWithCtx dSpec dVar dCtx)

unionCM :: ConstraintMap -> ConstraintMap -> ConstraintMap
unionCM = M.unionWith (M.unionWith S.union)

-- | A 'DepSpec' and it's context: the 'DepVar' where it was encountered and
--   its (optional) relevant 'DepGroup'
data DepWithCtx = DepWithCtx
    { dwcDepSpec :: DepSpec
    , dwcDepVar :: DepVar
    , dwcDepContext :: Maybe DepContext
    } deriving (Show, Eq, Ord, Generic, Hashable)

-- | A relevant context within which a 'DepSpec' was found
data DepContext
    = AnyCtx (NonEmpty (Either DepGroup DepSpec))
    | UseCtx (NonEmpty (Either DepGroup DepSpec)) UseFlag
    | NotUseCtx (NonEmpty (Either DepGroup DepSpec)) UseFlag
    deriving (Show, Eq, Ord, Generic, Hashable)

instance Printable DepContext where
    toString = \case
        AnyCtx ne -> toString $ OrGroup ne
        UseCtx ne uf -> toString $ UseGroup ne uf
        NotUseCtx ne uf -> toString $ NotUseCtx ne uf

-- | A 'HashMap' from a package/version pair to relevant dependencies and
--   their context. This is produced by looking up a particular
--   package + version + mode in the main 'ContextMap'.
type ResultMap = HashMap PkgWithVer (HashSet DepWithCtx)

-- Command line

type Debug = Any

data MatchMode
    = Matching
    | NonMatching
    deriving (Show, Eq, Ord)

data Mode
    = HelpMode
    | NormalMode (Last MatchMode) (Last Repository) Debug
    deriving (Show, Eq, Ord)

instance Semigroup Mode where
    HelpMode <> _ = HelpMode
    _ <> HelpMode = HelpMode
    NormalMode m1 r1 d1 <> NormalMode m2 r2 d2
        = NormalMode (m1 <> m2) (r1 <> r2) (d1 <> d2)

instance Monoid Mode where
    mempty = NormalMode mempty mempty mempty

checkArgs :: IO (NonEmpty (Either Package PkgWithVer), MatchMode, Repository, Debug)
checkArgs = do
    progName <- getProgName
    argv <- getArgs

    let goErr str = showHelp progName *> die ("error: " ++ str)
        foo = getOpt Permute options argv
    case foo of
        (_,_,es@(_:_)) -> goErr (intercalate " " es)

        (ms,as,_) -> case (mconcat ms, NE.nonEmpty as) of
            (HelpMode, _) -> showHelp progName *> exitSuccess
            (_, Nothing) -> goErr "At least one full package name (and optional \
                           \version) required"
            (NormalMode (Last mm) (Last mr) d, Just pStrs) ->
                case traverse parsePkg pStrs of
                    Failure ne -> goErr $ unlines $
                        (\(s,e) -> unwords
                            [ "Invalid package:", show s, "(", show e, ")" ]
                        ) <$> NE.toList ne
                    Success ps -> do
                        m <- case mm of
                            Just mode -> pure mode
                            Nothing -> detectMode ps
                        pure (ps, m, fromMaybe (Repository "haskell") mr, d)
  where
    showHelp progName = putStrLn (usageInfo (header progName) options)

    -- | In the event of an error, returns the original string and the error message(s)
    parsePkg
        :: String
        -> Validation
            (NonEmpty (String, Maybe String))
            (Either Package PkgWithVer)
    parsePkg s =
        let b = encodeString s
        in case (runParsable b, runParsable b) of
                    (Right spec, _) -> case spec of
                        VersionedDepSpec Nothing (VPkgEq p v) Nothing Nothing
                            -> pure $ Right $ PkgWithVer p v
                        UnversionedDepSpec Nothing p Nothing Nothing
                            -> pure $ Left p
                        _ -> let e = Just $ "Unsupported atom: " ++ show b
                             in failure (s,e)
                    (_, Right pwv) -> pure $ Right pwv
                    (Left e1, Left e2) ->
                        let es = NE.fromList [e1, e2]
                        in Failure $ (s,) <$> es

    header progName = unlines $ unwords <$>
        [ ["Usage:", progName, "[OPTION...]", "<cat/pkg[-ver]... >"]
        , []
        , ["This utility will scan a Gentoo repository and gather dependency information."]
        , []
        , ["--matching (default when no version is provided)"]
        , ["Looks for dependencies that match the given package atom."]
        , []
        , ["--non-matching (default when version is provided)"]
        , ["Looks for dependency constraints that would reject the provided"]
        , ["package/version. For example:", "`" ++ progName, "dev-haskell/network-3.2` would"]
        , ["match \"<dev-haskell/network-3.2\" as a problematic dependency."]
        ]

    options :: [OptDescr Mode]
    options =
        [ Option ['h'] ["help"] (NoArg HelpMode) "Show this help text"
        , Option ['r'] ["repo"]
            (ReqArg (\r -> NormalMode mempty (pure (Repository r)) mempty)
                "REPOSITORY"
            )
            "Limit to a repository (defaults to \"haskell\")"
        , Option [] ["debug"] (NoArg (NormalMode mempty mempty (Any True)))
            "Display debug information"
        , Option [] ["matching"]
            (NoArg (NormalMode (pure Matching) mempty mempty))
            "Look for matching dependencies"
        , Option [] ["non-matching"]
            (NoArg (NormalMode (pure NonMatching) mempty mempty))
            "Look for non-matching relevant dependencies"
        ]

    detectMode :: Foldable f => f (Either Package PkgWithVer) -> IO MatchMode
    detectMode ps
        | all isLeft  ps = pure NonMatching
        | all isRight ps = pure Matching
        | otherwise = do
            hPutStrLn stderr "Warning: Mix of versioned and non-versioned \
                             \packages were given on the command\n\
                             \line. Defaulting to \"non-matching mode\"."
            pure NonMatching

encodeString :: String -> BS.ByteString
encodeString = encodeUtf8 . T.pack
