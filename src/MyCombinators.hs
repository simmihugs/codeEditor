{-# LANGUAGE FunctionalDependencies #-}

module MyCombinators where

import Control.Lens (ALens')
import Data.Text (Text)

import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

import GHC.SyntaxHighlighter


-- -- | Whether a widget shows LineNumbers.
-- class CmbShowSyntax t where
--   showSyntax :: t
--   showSyntax = showSyntax_ True
--   showSyntax_ :: Bool -> t


-- | Whether a widget shows LineNumbers.
class CmbShowLineNumbers t where
  showLineNumbers :: t
  showLineNumbers = showLineNumbers_ True
  showLineNumbers_ :: Bool -> t


-- | Define syntax.
class CmbSyntax t where
  syntax :: (Maybe [(Token, Loc)], [(Token,Color)]) -> t


-- | Current Line color.
class CmbCurrentLineColor t where
  currentLineColor :: Color -> t

-- -- | Max line length.
-- class CmbMaxLineLength t where
--   maxLineLength :: Int -> t

