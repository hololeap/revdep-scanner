{-# Language DeriveAnyClass #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeApplications #-}

{-# Options_GHC -Wno-orphans #-}

module Main where

import Conduit
import Control.Monad
import Control.Monad.Trans.Accum
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Hashable
import Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as S
import           Data.HashSet (HashSet)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Conduit.Process
import Data.Monoid
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO (Handle, stdout, stderr, hPutStrLn)

import Text.Pretty.Simple (pPrintForceColor)

import Data.Parsable
import Distribution.Portage.Types

-- -Worphans is turned off for this
-- This should probably be moved to its own module, possibly in the
-- gentoo-utils package, or in a new package?
deriving newtype instance Hashable Category
deriving newtype instance Hashable PkgName
deriving newtype instance Hashable VersionNum
deriving newtype instance Hashable VersionLetter
deriving anyclass instance Hashable VersionSuffix
deriving newtype instance Hashable VersionSuffixNum
deriving newtype instance Hashable VersionRevision
deriving anyclass instance Hashable Version
deriving anyclass instance Hashable Slot
deriving newtype instance Hashable SubSlot
deriving newtype instance Hashable Repository
deriving anyclass instance Hashable ConstrainedDep
deriving anyclass instance Hashable Operator

main :: IO ()
main = do
    pquery <- runEnv pqueryPath
    (ps, mode, repoName, Any d) <- checkArgs

    when d $ print $ pquery : args repoName

    let f = if d then runTransparent else runOpaque

    (_, out, _) <- f pquery (args repoName)
    Right (m :: ConstraintMap) <- pure $ parseAll out

    when d $ pPrintForceColor m

    let ls = ps >>= \p -> do
            let r = lookupResults mode p m
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

prettyProblems
    :: Package
    -> HashMap Revdep (HashSet ParsedDep)
    -> NonEmpty String
prettyProblems p m
    | M.null m = NE.singleton
        $ toString p ++ ": No problematic packages found!"
    | otherwise
        = (toString p ++ ":")
        :| prettyResults m

prettyMatches
    :: Package
    -> HashMap Revdep (HashSet ParsedDep)
    -> NonEmpty String
prettyMatches p m
    | M.null m = NE.singleton
        $ toString p ++ ": No matches found!"
    | otherwise
        = (toString p ++ ":")
        :| prettyResults m

prettyResults :: HashMap Revdep (HashSet ParsedDep) -> [String]
prettyResults m =
          sortBy (compare `on` fst) (M.toList m) >>= \((c,n,v,sl,r),s) ->
                let p = Package c n (Just v) (Just sl) (Just r)
                    svs = sortBy cmp (S.toList s)
                in  [ "    " ++ toString p
                    , "        ( " ++ L.intercalate " " (map toStr svs) ++ " )"
                    ]
  where
    cmp :: ParsedDep -> ParsedDep -> Ordering
    cmp pd1 pd2 = case (pd1, pd2) of
        (Left (ConstrainedDep _ _ _ v1 _ _), Left (ConstrainedDep _ _ _ v2 _ _))
            -> v1 `compare` v2
        (Left _, Right _) -> GT
        (Right _, Left _) -> LT
        (_, _) -> EQ

    toStr :: ParsedDep -> String
    toStr = \case
        Left cd -> toString cd
        Right (c,n) -> toString $ Package c n Nothing Nothing Nothing

lookupResults
    :: MatchMode
    -> Package
    -> ConstraintMap
    -> HashMap Revdep (HashSet ParsedDep)
lookupResults mode p0@(Package c0 n0 _ _ _) m0 =
    case M.lookup (c0, n0) m0 of
        Just m -> foldr (M.unionWith S.union . go) M.empty (M.toList m)
        Nothing -> M.empty
  where
    go :: (Revdep, HashSet (ParsedDep)) -> HashMap Revdep (HashSet ParsedDep)
    go (r, s)
        | any check s = M.singleton r s
        | otherwise = M.empty

    check d = case (mode, d) of
        (Matching, Left cd) -> doesConstraintMatch cd p0
        (Matching, Right (c,n)) -> c == c0 && n == n0
        (NonMatching, Left cd) -> not (doesConstraintMatch cd p0)
        (NonMatching, Right _) -> False

-- Types

type ConstraintPkg = (Category, PkgName)
type Revdep = (Category, PkgName, Version, Slot, Repository)
type BasicDep = (Category, PkgName)
type ParsedDep = Either ConstrainedDep BasicDep

-- | Organized by @(Category, PkgName)@
--
--   The inner map is keyed by the reverse dependency and contains a set of
--   constraints that match the same @(Category, PkgName)@ as the outermost key.
type ConstraintMap = HashMap ConstraintPkg
    (HashMap Revdep (HashSet ParsedDep))

insertCM :: Revdep -> ParsedDep -> ConstraintMap -> ConstraintMap
insertCM revdep dep cmap0 =
    unionCM cmap0 $
        M.singleton (ccat,cpkg) (M.singleton revdep (S.singleton dep))
  where
    (ccat, cpkg) = case dep of
        Left (ConstrainedDep _ c p _ _ _) -> (c,p)
        Right bDep -> bDep

unionCM :: ConstraintMap -> ConstraintMap -> ConstraintMap
unionCM = M.unionWith (M.unionWith S.union)

-- Parsing

parseAll :: TL.Text -> Either (Maybe String) ConstraintMap
parseAll t = do
    cms <- traverse parseLine (TL.lines t)
    pure $ foldr unionCM M.empty cms
  where
    parseLine :: TL.Text -> Either (Maybe String) ConstraintMap
    parseLine l = do
        extractResult $ runParser lineParser (encodeLazyText l)

lineParser :: Parser String ConstraintMap
lineParser = do
    parser @ConstrainedDep >>= \case -- parser from Data.Parsable
        ConstrainedDep Equal rdCat rdPkg rdVer (Just rdSlot) (Just rdRepo) -> do
            let revdep = (rdCat, rdPkg, rdVer, rdSlot, rdRepo)
            cds <- bruteForce
            pure $ foldr (insertCM revdep) M.empty cds
        cd -> err $ "Invalid ConstrainedDep: " ++ show cd
  where
    -- Start with char 0, see if it's a valid ConstrainedDep /or/ Package.
    -- Try next char, see if it's a valid ConstrainedDep /or Package.
    -- etc...
    bruteForce :: Parser String [ParsedDep]
    bruteForce = choice
        [ try $ (:) <$> (Left <$> parser @ConstrainedDep) <*> bruteForce
        , try $ do
--             Package c n Nothing _ _ <- parser
            parser >>= \case
                Package c n Nothing _ _ ->
                    (Right (c,n) :) <$> bruteForce
                p -> err $ "Invalid Package: " ++ show p
        , try $ [] <$ eof
        , anyChar *> bruteForce
        ]

-- Environment stuff

type Env = AccumT (First FilePath) IO

runEnv :: Env a -> IO a
runEnv = flip evalAccumT mempty

-- | Find the path to the @pquery@ executable or throw an error. Caches the
--   result in the case of a success.
pqueryPath :: Env FilePath
pqueryPath = look >>= \case
    First (Just p) -> pure p
    First Nothing -> liftIO (findExecutable "pquery") >>= \case
        Just p -> add (pure p) *> pure p
        Nothing -> liftIO $
            die "Could not find pquery executable. Install sys-apps/pkgcore first."

-- Util stuff

-- | Run a command and capture stdout and stderr
runOpaque
    :: FilePath -- ^ executable path
    -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
    -> IO (ExitCode, TL.Text, TL.Text)
runOpaque exe args
    = sourceProcessWithStreams
        (proc exe args)
        (pure ())
        (decodeUtf8LenientC .| sinkLazy)
        (decodeUtf8LenientC .| sinkLazy)

-- | Run a command and dump stdout to @stdout@, stderr to @stderr@, also
--   capturing both streams.
runTransparent
    :: FilePath -- ^ executable path
    -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
    -> IO (ExitCode, TL.Text, TL.Text)
runTransparent exe args
    = sourceProcessWithStreams (proc exe args) { delegate_ctlc = True }
            (pure ()) (transSink stdout) (transSink stderr)
  where
    transSink :: Handle -> ConduitT BS.ByteString Void IO TL.Text
    transSink h = iterMC (BS.hPut h) .| decodeUtf8LenientC .| sinkLazy

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

checkArgs :: IO (NonEmpty Package, MatchMode, Repository, Debug)
checkArgs = do
    progName <- getProgName
    argv <- getArgs
    let goErr str = showHelp progName *> die ("error: " ++ str)

    case getOpt Permute options argv of
        (_,_,es@(_:_)) -> goErr (intercalate " " es)

        (ms,as,_) -> case (mconcat ms, NE.nonEmpty as) of
            (HelpMode, _) -> showHelp progName *> exitSuccess
            (_, Nothing) -> goErr "At least one full package name (and optional \
                           \version) required"
            (NormalMode (Last mm) (Last mr) d, Just pStrs) ->
                case traverse (runParsable . encodeString) pStrs of
                    Left e -> goErr $
                        "Invalid package: " ++ show e
                    Right ps -> do
                        m <- case mm of
                            Just mode -> pure mode
                            Nothing -> detectMode ps
                        pure (ps, m, fromMaybe (Repository "haskell") mr, d)
  where
    showHelp progName = putStrLn (usageInfo (header progName) options)

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

    detectMode :: Foldable f => f Package -> IO MatchMode
    detectMode ps
        | all (isJust . getVersion) ps = pure NonMatching
        | all (isNothing . getVersion) ps = pure Matching
        | otherwise = do
            hPutStrLn stderr "Warning: Mix of versioned and non-versioned \
                             \packages were given on the command\n\
                             \line. Defaulting to \"non-matching mode\"."
            pure NonMatching

encodeString :: String -> BS.ByteString
encodeString = encodeUtf8 . T.pack

encodeLazyText :: TL.Text -> BS.ByteString
encodeLazyText = encodeUtf8 . TL.toStrict
