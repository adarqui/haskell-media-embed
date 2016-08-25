{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Media.Embed.IFrame (
  IFrame (..),
  defaultIFrame
) where



import           Control.DeepSeq (NFData)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)



data IFrame = IFrame {
  iframeSrc     :: Text,
  iframeHeight  :: !(Maybe Int),
  iframeWidth   :: !(Maybe Int),
  iframeBoarder :: !(Maybe Bool)
} deriving (Show, Eq, Generic, Typeable, NFData)



defaultIFrame :: IFrame
defaultIFrame = IFrame {
  iframeSrc     = "",
  iframeHeight  = Nothing,
  iframeWidth   = Nothing,
  iframeBoarder = Nothing
}
