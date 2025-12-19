{-# Language DataKinds #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module RevdepScanner.Display
    ( prettyProblems
    , prettyMatches
    ) where

import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.NonEmpty as NEM
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set (Set)
import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

import RevdepScanner.Types
import qualified RevdepScanner.Types.ContextMap as CtxMap
import           RevdepScanner.Types.ContextMap (ContextMap)
import RevdepScanner.Types.DepMap
import RevdepScanner.Types.ResultMap

prettyProblems
    :: forall mode. AllDepMaps mode '[ IsBool ]
    => Package
    -> EvaluatedResultMap mode
    -> Doc AnsiStyle
prettyProblems p m
    | null m
        = toDoc p <> colon <> space <> pretty "No problematic packages found!"
    | otherwise
        = annotate (color Magenta) (toDoc p <> colon)
            <> line
            <> indent 4 (prettyResults m)

prettyMatches
    :: forall mode. AllDepMaps mode '[ IsBool ]
    => Package
    -> EvaluatedResultMap mode
    -> Doc AnsiStyle
prettyMatches p m
    | null m
        = toDoc p <> colon <> space <> pretty "No matches found!"
    | otherwise
        = annotate (color Magenta) (toDoc p <> colon)
            <> line
            <> indent 4 (prettyResults m)

prettyResults
    :: forall m. AllDepMaps m '[ IsBool ]
    => EvaluatedResultMap m
    -> Doc AnsiStyle
prettyResults = vsep . (go <=< M.toList)
  where
    go :: (PkgWithVer, NEMap DepVar (ContextMap ('Just m)))
        -> [Doc AnsiStyle]
    go (pwv, m0) =
        fmap (annotate (color Cyan)) $ toDoc pwv : do
            (dv, cm) <- NEL.toList $ NEM.toList m0
            fmap (annotate (color Green) . indent 4) $ toDoc dv : do
                CtxMap.toList cm >>= pure . annotate (color Blue) . indent 4 . \case
                    Left (Just ctx, nm) ->
                        prettyContext $ Left (ctx, nm)
                    Left (Nothing, nm) ->
                        highlightDepMap highlightStyle nm
                    Right (ctx, om) ->
                        prettyContext $ Right (ctx, om)

-- | Pretty print a 'DepContext' or 'OrContext', highlighting any 'DepSpec's
--   that are in the given evaluated dependency set.
prettyContext
    :: forall mode. AllDepMaps mode '[ IsBool ]
    => Either
            (DepContext, DepMap ('Just mode))
            (OrContext, OrGroupMap ('Just mode))
    -> Doc AnsiStyle
prettyContext = \case
    Left (UseCtx ne uf, m) ->
        highlightGroup (toHSet m) highlightStyle (UseGroup ne uf)
    Left (NotUseCtx ne uf, m) ->
        highlightGroup (toHSet m) highlightStyle (NotUseGroup ne uf)
    Right (OrCtx ne, m) ->
        highlightGroup (toHSet m) highlightStyle (OrGroup ne)
  where
    toHSet
        :: forall t. (IsDepMap t, IsBool (MatchLogic t ('Just mode)))
        => t ('Just mode)
        -> Set DepSpec
    toHSet = S.fromList . NEL.toList . NEM.keys . unwrapDepMap

-- | Pretty print an evaluated dependency set, highlighting all elements.
highlightDepMap
    :: forall t mode. IsDepMap t
    => AnsiStyle
    -> t ('Just mode)
    -> Doc AnsiStyle
highlightDepMap style
    = encloseSep (lparen <> space) (space <> rparen) space
    . fmap (annotate style . toDoc)
    . NEL.toList
    . NEM.keys
    . unwrapDepMap

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
