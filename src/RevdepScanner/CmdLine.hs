module RevdepScanner.CmdLine
    ( checkArgs
    , MatchMode(..)
    , Mode(..)
    , Repository
    , Debug
    , parsePkg
    ) where

import Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO (stderr, hPutStrLn)

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types
import Distribution.Gentoo.Utils.Pquery

import RevdepScanner.Types
import RevdepScanner.Util (encodeString)

type Debug = Any

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

checkArgs :: IO (NonEmpty CmdlinePkg, MatchMode, Repository, Debug)
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

    detectMode :: Foldable f => f CmdlinePkg -> IO MatchMode
    detectMode ps
        | all isPkg ps = pure Matching
        | all isPwv ps = pure NonMatching
        | otherwise = do
            hPutStrLn stderr "Warning: Mix of versioned and non-versioned \
                             \packages were given on the command\n\
                             \line. Defaulting to \"non-matching mode\"."
            pure NonMatching
      where
        isPkg :: CmdlinePkg -> Bool
        isPkg (CmdlinePackage _) = True
        isPkg _ = False

        isPwv :: CmdlinePkg -> Bool
        isPwv (CmdlinePkgWithVer _) = True
        isPwv _ = False

-- | Parse a package (with or without version from the command line). This
--   can be any of these valid inputs:
--
--   * 'Package' (@category/package@)
--   * 'PkgWithVer' (@category/package-ver@)
--   * 'VPkgEq' (@=category/package-ver@)
--
--   Any other input will create an error which will be accumulated in the
--   'Validation'.
parsePkg
    :: String
    -> Validation
        (NonEmpty (String, Maybe String))
        CmdlinePkg
parsePkg s =
    case runParsable (encodeString s) of
        Right p -> pure p
        Left e -> Failure $ NE.singleton (s,e)
