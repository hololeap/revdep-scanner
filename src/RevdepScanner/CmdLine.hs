module RevdepScanner.CmdLine
    ( checkArgs
    , MatchMode(..)
    , Mode(..)
    , Repository
    , Debug
    ) where

import Data.Either (isLeft, isRight)
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
