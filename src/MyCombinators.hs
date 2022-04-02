{-# LANGUAGE FunctionalDependencies #-}

module MyCombinators where

import Control.Lens (ALens')
import Data.Text (Text)

import Monomer.Core.StyleTypes
import Monomer.Core.WidgetTypes
import Monomer.Event.Types
import Monomer.Graphics.Types

import GHC.SyntaxHighlighter


-- | Whether a widget shows LineNumbers.
class CmbShowLineNumbers t where
  showLineNumbers :: t
  showLineNumbers = showLineNumbers_ True
  showLineNumbers_ :: Bool -> t


-- | Linenumber background color.
class CmbLineNumberBackgroundColor t where
  lineNumberBackgroundColor :: Color -> t


-- | Linenumber number color.
class CmbLinenumberNumberColor t where
  lineNumberNumberColor :: Color -> t


-- | Linenumber number color.
class CmbLinenumberNumberHighlightColor t where
  lineNumberNumberHighlightColor :: Color -> t


-- | Define syntax.
class CmbSyntax t where
  syntax :: (Maybe [(Token, Loc)], [(Token,Color)]) -> t


-- | Current Line color.
class CmbCurrentLineColor t where
  currentLineColor :: Color -> t


