{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains types for embedding instagram media
--

module Web.Media.Embed.Instagram (
  simpleInstagramEmbedToIFrame
) where



import           Control.DeepSeq          (NFData)
import           Data.Monoid              ((<>))
import qualified Data.Text                as Text
import           Data.Text                (Text)
import           Data.Typeable            (Typeable)
import           GHC.Generics             (Generic)

import           Web.Media.Embed.Internal
import           Web.Media.Embed.IFrame



-- | Really simple function to keep the url basically the same, but outfitting it for instagram embedding
--
simpleInstagramEmbedToIFrame :: Text -> IFrame -> IFrame
simpleInstagramEmbedToIFrame link IFrame{..} =
  IFrame {
    iframeSrc       = instagram_src,
    iframeName      = iframeName,
    iframeHeight    = iframeHeight,
    iframeWidth     = iframeWidth,
    iframeBoarder   = iframeBoarder,
    iframeScrolling = iframeScrolling
  }
  where
  instagram_src = link
