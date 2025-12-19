{-# Language TypeFamilies #-}

module Main (main) where

import Conduit
import Control.Monad
import Data.List as L
import qualified Data.HashMap.Strict as M
import Data.Monoid
import qualified ListT
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.IO (stdout)

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
import RevdepScanner.Types.DepMap

main :: IO ()
main = do
    (ps, mode, repo, Any d) <- checkArgs

    vDeps <- runExeEnv $ do
        when d $ liftIO $ print $ unwords $ "pquery" : args repo
        getPqueryDump ["--repo", unwrapRepository repo] CM.buildCMap

    liftMatchMode mode $ \(lmode :: LiftedMatchMode mode) -> case vDeps of
        Failure es -> error $ "Parsing failure: " ++ show es
        Success deps -> do
            let (m :: ConstraintMap) = foldl' CM.union M.empty deps

            when d $ pPrintForceColor deps

            docs <- ListT.toList $ do
                cp <- ListT.fromFoldable ps
                let p = cmdlinePkgPackage cp
                    r = lookupResults lmode cp m
                when d $ pPrintForceColor r
                pure $ case lmode of
                    LMatching -> prettyMatches p r
                    LNonMatching -> prettyProblems p r

            renderIO stdout $ layoutPretty defaultLayoutOptions $ vsep docs
            putStrLn ""
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
