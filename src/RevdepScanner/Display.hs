{-# Language LambdaCase #-}

module RevdepScanner.Display
    ( prettyProblems
    , prettyMatches
    , prettyResults
    ) where

import Data.Foldable (toList)
import Data.Function (on)
import Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import RevdepScanner.Types

prettyProblems
    :: Package
    -> ResultMap
    -> NonEmpty String
prettyProblems p m
    | null m = NE.singleton
        $ toString p ++ ": No problematic packages found!"
    | otherwise
        = (toString p ++ ":")
        :| prettyResults m

prettyMatches
    :: Package
    -> ResultMap
    -> NonEmpty String
prettyMatches p m
    | null m = NE.singleton
        $ toString p ++ ": No matches found!"
    | otherwise
        = (toString p ++ ":")
        :| prettyResults m

prettyResults :: ResultMap -> [String]
prettyResults m =
    sortBy (compare `on` fst) (M.toList m) >>= \((PkgWithVer p v),s) ->
        let p' = VPkgEq p v
            svs = sortBy cmp (toList s)
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
