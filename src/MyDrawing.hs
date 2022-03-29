{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module MyDrawing where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (^?), (^?!), (.~), non)
import Control.Monad (forM_, void, when)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Sequence as S

import Monomer.Core
import Monomer.Graphics.Types

import qualified Monomer.Common.Lens as L
import qualified Monomer.Core.Lens as L
import qualified Monomer.Graphics.Lens as L

import Monomer.Graphics.Util

import GHC.SyntaxHighlighter
import Monomer.Widgets.Util.Drawing (drawRect)


drawTextSyntaxed :: Renderer -> StyleState -> [(Token, Loc)] -> [(Token,Color)] -> S.Seq TextLine -> IO ()
drawTextSyntaxed renderer style syntaxTree syntaxMap textLines = do
  draw 1 textLines
  where
    draw index textLines
      | textLines == S.empty = return ()
      | otherwise            = do
          drawTextLineSyntaxed renderer style index (textLines `S.index` 0)
          draw (succ index) (S.drop 1 textLines)

    drawTextLineSyntaxed renderer style  lineNumber textLine = do
      let syntax = filter (\(_,Loc s _ _ _) -> s == lineNumber) syntaxTree
      drawTextWithSyntax syntax textLine
      when underline $ do
        drawLine renderer (Point (delta'+tx) uy) (Point (delta'+tr) uy) lw (Just fontColor)
      when overline $ do
        drawLine renderer (Point (delta'+tx) oy) (Point (delta'+tr) oy) lw (Just fontColor)
      when throughline $ do
        drawLine renderer (Point (delta'+tx) hy) (Point (delta'+tr) hy) lw (Just fontColor)
          where
            delta' = maybe 0 _glpXMin xx
              where
                xx = S.lookup 0 _tlGlyphs
            fontColor = styleFontColor style
            TextLine{..} = textLine
            TextMetrics asc desc _ _ = _tlMetrics
            alignV = styleTextAlignV style
            Rect tx ty tw th = _tlRect
            tr = tx + tw
            oy = ty
            uy = by + 1.5 * lw
            lw = max 1.5 (unFontSize _tlFontSize / 20)
            hy = by - asc * 0.35
            by = ty + th + offset
            offset
              | alignV == ATBaseline = 0
              | otherwise = desc
            underline = style ^?! L.text . non def . L.underline . non False
            overline = style ^?! L.text . non def . L.overline . non False
            throughline = style ^?! L.text . non def . L.throughline . non False
            

    drawTextWithSyntax :: [(Token, Loc)] -> TextLine -> IO ()
    drawTextWithSyntax [] _            = return ()
    drawTextWithSyntax (x:xs) textLine = do
      let (token,Loc startLine startColumn endLine endColumn) = x
      let TextLine{..} = textLine
      let TextMetrics asc desc _ _ = _tlMetrics
      let Rect tx ty tw th = _tlRect
      let tr = tx + tw
  
      let fontColor = styleFontColor style
      let alignV = styleTextAlignV style

      let offset
            | alignV == ATBaseline = 0
            | otherwise = desc
      let lw = max 1.5 (unFontSize _tlFontSize / 20)
      let by = ty + th + offset
      let uy = by + 1.5 * lw
      let oy = ty
      let hy = by - asc * 0.35

      let x2 = maybe 50 _glpXMin (S.lookup (pred startColumn) _tlGlyphs)

      let text = _tlText
      let textPart
            | startLine==endLine = T.drop (pred startColumn) $ T.take (pred endColumn) _tlText
            | otherwise          = T.drop (pred startColumn) _tlText
      
      --let txtOrigin = Point (tx+x2) by
      let txtOrigin = Point (fromIntegral (floor (tx + x2))) by
      
      setFillColor renderer (fromMaybe (rgbHex "#ffffff") (lookup token syntaxMap))
      renderText renderer txtOrigin _tlFont _tlFontSize _tlFontSpaceH textPart

      drawTextWithSyntax xs textLine


-- | Draws a line with the given width and color.
drawLine
  :: Renderer     -- ^ The renderer.
  -> Point        -- ^ The start point.
  -> Point        -- ^ The end point.
  -> Double       -- ^ The line width.
  -> Maybe Color  -- ^ The color. If Nothing, the line will not be drawn.
  -> IO ()        -- ^ The resulting action.
drawLine _ _ _ _ Nothing = pure ()
drawLine renderer p1 p2 width (Just color) = do
  beginPath renderer
  setStrokeColor renderer color
  setStrokeWidth renderer width
  renderLine renderer p1 p2
  stroke renderer


