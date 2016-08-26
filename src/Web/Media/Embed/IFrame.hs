{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Media.Embed.IFrame (
    IFrameScrolling (..)
  , IFrame (..)
  , defaultIFrame
) where



import           Control.DeepSeq          (NFData)
import           Data.Text                (Text)
import           Data.Typeable            (Typeable)
import           GHC.Generics             (Generic)

import           Web.Media.Embed.Internal



data IFrameScrolling
  = ScrollingNo
  | ScrollingYes
  | ScrollingAuto
  deriving (Show, Eq, Generic, Typeable, NFData)

instance ToParam IFrameScrolling where
  toParam scrolling =
    case scrolling of
      ScrollingNo   -> "no"
      ScrollingYes  -> "yes"
      ScrollingAuto -> "auto"



data IFrame = IFrame {
  iframeSrc       :: !Text,
  iframeName      :: !(Maybe Text),
  iframeHeight    :: !(Maybe Int),
  iframeWidth     :: !(Maybe Int),
  iframeBoarder   :: !(Maybe Bool),
  iframeScrolling :: !(Maybe IFrameScrolling)
} deriving (Show, Eq, Generic, Typeable, NFData)



defaultIFrame :: IFrame
defaultIFrame = IFrame {
  iframeSrc       = "",
  iframeName      = Nothing,
  iframeHeight    = Nothing,
  iframeWidth     = Nothing,
  iframeBoarder   = Nothing,
  iframeScrolling = Nothing
}
