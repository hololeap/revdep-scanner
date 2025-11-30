{-# Language DataKinds #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module RevdepScanner.Display
    ( prettyProblems
    , prettyMatches
    ) where

import Control.Monad.Reader
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Set.NonEmpty as NES
import qualified ListT
import           ListT (cons)
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import RevdepScanner.Types
import RevdepScanner.Types.DepSet
import RevdepScanner.Types.ResultMap

prettyProblems
    :: MonadReader (LiftedMatchMode mode) m
    => Package
    -> EvaluatedResultMap mode
    -> m (Doc AnsiStyle)
prettyProblems p m
    | null m = pure
        $ toDoc p <> colon <> space <> pretty "No problematic packages found!"
    | otherwise = do
        r <- prettyResults m
        pure $ annotate (color Magenta) (toDoc p <> colon)
            <> line
            <> indent 4 r

prettyMatches
    :: MonadReader (LiftedMatchMode mode) m
    => Package
    -> EvaluatedResultMap mode
    -> m (Doc AnsiStyle)
prettyMatches p m
    | null m = pure
        $ toDoc p <> colon <> space <> pretty "No matches found!"
    | otherwise = do
        r <- prettyResults m
        pure $ annotate (color Magenta) (toDoc p <> colon)
            <> line
            <> indent 4 r

prettyResults
    :: MonadReader (LiftedMatchMode mode) m
    => EvaluatedResultMap mode
    -> m (Doc AnsiStyle)
prettyResults
    = fmap vsep
    . ListT.toList
    . (go <=< ListT.fromFoldable . M.toList)
  where
    go (pwv, m0) =
        fmap (annotate (color Cyan)) $ toDoc pwv `cons` do
            (dv, m1) <- ListT.fromFoldable $ NEM.toList m0
            fmap (annotate (color Green) . indent 4) $ toDoc dv `cons` do
                (mdc, ds) <- ListT.fromFoldable $ NEM.toList m1
                fmap (annotate (color Blue) . indent 4) $ lift $ case mdc of
                    Just ctx -> prettyContext ds ctx
                    Nothing -> case ds of
                        Left s -> highlightSet highlightStyle s
                        Right s -> highlightSet highlightStyle s

-- | Pretty print a 'DepContext', highlighting any 'DepSpec's that are in
--   the given dependency set and tagged with @True@.
prettyContext
    :: forall mode m. MonadReader (LiftedMatchMode mode) m
    => Either (DepOrGroup' ('Just mode)) (DepSet' ('Just mode))
    -> DepContext
    -> m (Doc AnsiStyle)
prettyContext ds dc = case (dc, ds) of
    (OrCtx ne, Left s) -> do
        hSet <- toHSet s
        pure $ highlightGroup hSet highlightStyle (OrGroup ne)
    (OrCtx _, Right _) -> error $ "OrCtx should not match (Right DepSet)"
    (UseCtx _ _, Left _) -> error $ "UseCtx should not match (Left DepOrGroup)"
    (NotUseCtx _ _, Left _) -> error $ "NotUseCtx should not match (Left DepOrGroup)"
    (UseCtx ne uf, Right s) -> do
        hSet <- toHSet s
        pure $ highlightGroup hSet highlightStyle (UseGroup ne uf)
    (NotUseCtx ne uf, Right s) -> do
        hSet <- toHSet s
        pure $ highlightGroup hSet highlightStyle (NotUseGroup ne uf)
  where
    toHSet
        :: forall t. IsDepSet t
        => t ('Just mode)
        -> m (Set DepSpec)
    toHSet s
        = (ask >>=)
        $ \mode -> pure
        $ S.map snd
        $ NES.filter fst
        $ NES.map (first (lowerBool @t mode)) (unwrapDepSet s)

-- | Pretty print a dependency set, highlighting any that have been tagged
--   with @True@.
highlightSet
    :: forall t mode m. (MonadReader (LiftedMatchMode mode) m, IsDepSet t)
    => AnsiStyle
    -> t ('Just mode)
    -> m (Doc AnsiStyle)
highlightSet style set = ask >>= \mode ->
    let s = NES.map (first (lowerBool @t mode)) (unwrapDepSet set)
    in pure $ encloseSep
        (lparen <> space)
        (space <> rparen)
        space
        $ let f (b, spec) =
                if b then annotate style (toDoc spec) else toDoc spec
          in f <$> NEL.toList (NES.toList s)

-- | Pretty print a 'DepGroup', highlighting any 'DepSpec' that is in the
--   given @'Set' 'DepSpec'@.
highlightGroup :: Set DepSpec -> AnsiStyle -> DepGroup -> Doc AnsiStyle
highlightGroup hSet style = \case
    (AndGroup ne)
        -> encloseSep
            (lparen <> space)
            (space <> rparen)
            space
            $ highlightNE ne
    (OrGroup ne)
        -> encloseSep
            (pipe <> pipe <> space <> lparen <> space)
            (space <> rparen)
            space
            $ highlightNE ne
    (UseGroup ne u)
        -> encloseSep
            (toDoc u <> pretty "?" <> space <> lparen <> space)
            (space <> rparen)
            space
            $ highlightNE ne
    (NotUseGroup ne u)
        -> encloseSep
            (pretty "!" <> toDoc u <> pretty "?" <> space <> lparen <> space)
            (space <> rparen)
            space
            $ highlightNE ne
  where
    highlightNE
        :: NonEmpty (Either DepGroup DepSpec)
        -> [Doc AnsiStyle]
    highlightNE
        = NEL.toList
        . fmap (either (highlightGroup hSet style) (highlightSpec hSet style))

-- | Pretty print a 'DepSpec', highlighting any that are in the
--   given @'Set' 'DepSpec'@.
highlightSpec :: Set DepSpec -> AnsiStyle -> DepSpec -> Doc AnsiStyle
highlightSpec hSet style spec
    | spec `S.member` hSet = annotate style (toDoc spec)
    | otherwise = toDoc spec

-- | The default style for highlighting (currently red and bold)
highlightStyle :: AnsiStyle
highlightStyle = color Red <> bold

toDoc :: Printable a => a -> Doc ann
toDoc = pretty . toString
