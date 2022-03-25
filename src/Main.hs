{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding ( readFile )
import Control.Lens ( (&), (^.), (%~), makeLenses )
import Data.Maybe ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO ( readFile )
import Monomer


import qualified Monomer.Lens as L


import qualified MyTextArea as My
import qualified MyCombinators as Cmb


import GHC.SyntaxHighlighter
    ( tokenizeHaskellLoc,
      Token(OtherTok, KeywordTok, PragmaTok, SymbolTok, VariableTok,
            ConstructorTok, OperatorTok, CharTok, StringTok, IntegerTok,
            RationalTok, CommentTok, SpaceTok) )
import Monomer (CmbHlColor(hlColor))


data AppModel = AppModel {
  _sampleText    :: Text,
  _haskellText   :: Text,
  _syntax        :: Bool,
  _lineNumbers   :: Bool,
  _currentLine   :: Bool,
  _modelTextSize :: Double 
} deriving (Eq, Show)

data AppEvent = AppInit
              | AppShowSyntax
              | AppLineNumbers
              | AppCurrentLine
              deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  syntaxTree = tokenizeHaskellLoc (model ^. haskellText)

  syntaxMap = [(KeywordTok    ,rgbHex "#03dffc")
              , (PragmaTok     ,rgbHex "#ffb638")
              , (SymbolTok     ,rgbHex "#e1e81c")
              , (VariableTok   ,rgbHex "#ffffff")
              , (ConstructorTok,rgbHex "#03fc24")    
              , (OperatorTok   ,rgbHex "#e1e81c")
              , (CharTok       ,rgbHex "#ff2200")
              , (StringTok     ,rgbHex "#ff2200")
              , (IntegerTok    ,rgbHex "#ffffff")
              , (RationalTok   ,rgbHex "#ffffff")
              , (CommentTok    ,rgbHex "#fc9003")
              , (SpaceTok      ,rgbHex "#ffffff")
              , (OtherTok      ,rgbHex "#ff38e8")]
  
  widgetTree = vstack [ hstack [button (if model ^. syntax then "Syntax off" else "Syntax") AppShowSyntax
                                `styleBasic` [width 150]
                               , spacer
                               , button (if model ^. syntax then "LineNumbers off" else "LineNumbers") AppLineNumbers
                                `styleBasic` [width 150]
                               , spacer
                               , button (if model ^. currentLine then "Highlight off" else "Highlight") AppCurrentLine
                                `styleBasic` [width 150]
                               , filler]
                      , spacer
                      , My.textArea_ haskellText
                        (Cmb.showLineNumbers_ (model ^. lineNumbers) : [Cmb.currentLineColor (rgbHex "#358770") | model ^. currentLine] ++ [Cmb.syntax (syntaxTree,syntaxMap) | model ^. syntax])
                        `styleBasic` [bgColor $ rgbHex "#000000"
                                     , textColor $ rgbHex "#ffffff"
                                     , sndColor $ rgbHex "#358770"
                                     , textFont "Mono"
                                     , textSize (model ^. modelTextSize)
                                     , hlColor $ rgbHex "#5b388c"]
                      ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit        -> []
  AppShowSyntax  -> [Model $ model & syntax %~ not]
  AppLineNumbers -> [Model $ model & lineNumbers %~ not]
  AppCurrentLine -> [Model $ model & currentLine %~ not]

main :: IO ()
main = do
  haskellText' <- readFile "./src/Main.hs"
  let model = AppModel "abcdefghijklmnopqrstuvwxyz\nabcdefghijklmnopqrstuvwxyz" haskellText' True True True 15
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./assets/images/icon.bmp",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Mono" "./assets/fonts/RobotoMono-Regular.ttf",
      appInitEvent AppInit
      ]
    
