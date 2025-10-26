module Main (main) where

import Conduit
import Control.Monad
import Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as M
import Data.Monoid

import Text.Pretty.Simple (pPrintForceColor)

import Distribution.Portage.Types
import Distribution.Gentoo.Utils.Exe
import Distribution.Gentoo.Utils.Pquery

import RevdepScanner.CmdLine
import RevdepScanner.Display
import RevdepScanner.Logic
import RevdepScanner.Types
import           RevdepScanner.Types.ConstraintMap (ConstraintMap)
import qualified RevdepScanner.Types.ConstraintMap as CM

main :: IO ()
main = do
    (ps, mode, repo, Any d) <- checkArgs

    vDeps <- runExeEnv $ do
        when d $ liftIO $ print $ unwords $ "pquery" : args repo
        getPqueryDump ["--repo", unwrapRepository repo] CM.buildCMap

    case vDeps of
        Failure es -> error $ "Parsing failure: " ++ show es
        Success deps -> do
            let (m :: ConstraintMap) = foldl' CM.union M.empty deps

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
