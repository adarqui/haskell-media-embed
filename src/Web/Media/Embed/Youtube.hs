{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains types for embedding youtube media
-- reference: https://developers.google.com/youtube/player_parameters
--

module Web.Media.Embed.Youtube (
    YoutubeSource (..)
  , YoutubeOnOff (..)
  , YoutubeColor (..)
  , YoutubeControls (..)
  , YoutubeTheme (..)
  , YoutubeEmbed (..)
  , defaultYoutubeEmbed
  , youtubeEmbedBaseURL
  , youtubeEmbedToURL
) where



import           Control.DeepSeq          (NFData)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Data.Typeable            (Typeable)
import           GHC.Generics             (Generic)

import           Web.Media.Embed.Internal



data YoutubeSource
  = VideoID Text       -- ^ VIDEO_ID
  | VideoURL Text      -- ^ https://www.youtube.com/embed/VIDEO_ID
  | VideoPlaylist Text -- ^ https://www.youtube.com/embed?listType=playlist&list=PLAYLIST (PLXXX)
  | VideoUser Text     -- ^ https://www.youtube.com/embed?listType=user_uploads&list=USERNAME
  | VideoSearch Text   -- ^ https://www.youtube.com/embed?listType=search&list=QUERY
  deriving (Show, Eq, Generic, Typeable, NFData)



data YoutubeOnOff
  = On
  | Off
  deriving (Show, Eq, Generic, Typeable, NFData)

instance ToParam YoutubeOnOff where
  toParam On  = "1"
  toParam Off = "0"



data YoutubeColor
  = Red
  | White
  deriving (Show, Eq, Generic, Typeable, NFData)

instance ToParam YoutubeColor where
  toParam Red   = "red"
  toParam White = "white"



data YoutubeControls
  = ControlsNone
  | ControlsIFrame
  | ControlsFlash
  deriving (Show, Eq, Generic, Typeable, NFData)

instance ToParam YoutubeControls where
  toParam ControlsNone   = "0"
  toParam ControlsIFrame = "1"
  toParam ControlsFlash  = "2"



data YoutubeTheme
  = Dark
  | Light
  deriving (Show, Eq, Generic, Typeable, NFData)

instance ToParam YoutubeTheme where
  toParam Dark  = "dark"
  toParam Light = "light"



data YoutubeEmbed = YoutubeEmbed {
  youtubeSrc            :: !YoutubeSource,
  youtubeHeight         :: !(Maybe Int),
  youtubeWidth          :: !(Maybe Int),
  youtubeAutoPlay       :: !(Maybe YoutubeOnOff),     -- ^ This parameter specifies whether the initial video will
                                                      -- automatically start to play when the player loads.
                                                      -- default: Off

  youtubeCCLoadPolicy   :: !(Maybe YoutubeOnOff),     -- ^ Setting the parameter's value to On causes closed
                                                      -- captions to be shown by default, even if the user has
                                                      -- turned captions off. The default behavior is based on user preference.
                                                      -- default: Off

  youtubeColor          :: !(Maybe YoutubeColor),
  youtubeControls       :: !(Maybe YoutubeControls),  -- ^ This parameter indicates whether the video player controls are displayed.
  youtubeDisableKb      :: !(Maybe YoutubeOnOff),     -- ^ On enables keyboard control, Off disables it
                                                      -- default: Off

  youtubeEnableJsApi    :: !(Maybe YoutubeOnOff),     -- ^ On enables the player to be controlled via iframe or javascript player api calls
                                                      -- default: Off

  youtubeEnd            :: !(Maybe Int),              -- ^ This parameter specifies the time, measured in seconds from
                                                      -- the start of the video, when the player should stop playing the video.
                                                      -- The parameter value is a positive integer.

  youtubeFs             :: !(Maybe YoutubeOnOff),     -- ^ Off: prevents full screen. On: allows full screen.
                                                      -- default: On

  youtubeHl             :: !(Maybe Text),             -- ^ Sets the player's interface language. ie: fr, fr-ca
  youtubeIvLoadPolicy   :: !(),                       -- ^ video annotations.
  youtubeLoop           :: !(Maybe YoutubeOnOff),     -- ^ Off: video doesn't loop. On: video loops indefinitely.
  youtubeModestBranding :: !(Maybe YoutubeOnOff),     -- ^ Off: show youtube logo. On: don't show youtube logo.
                                                      -- default: Off

  youtubeOrigin         :: !(Maybe Text),             -- ^ This parameter provides an extra security measure for
                                                      -- the IFrame API and is only supported for IFrame embeds.

  youtubePlaysInline    :: !(Maybe YoutubeOnOff),     -- ^ This parameter controls whether videos play inline or fullscreen
                                                      -- in an HTML5 player on iOS.
                                                      -- Off: causes full screen playback. On: causes inline playback.
                                                      -- default: Off

  youtubeRel            :: !(Maybe YoutubeOnOff),     -- ^ Indicates whether the player should show related videos when playback
                                                      -- of the initial video ends.
                                                      -- Off: don't show related videos. On: show related videos.
                                                      -- default: On

  youtubeShowInfo       :: !(Maybe YoutubeOnOff),     -- ^ Setting the parameter's value to Off causes the player to
                                                      -- not display information like the video title and uploader before
                                                      -- the video starts playing.
                                                      -- default: On

  youtubeStart          :: !(Maybe Int),              -- ^ This parameter causes the player to begin playing the video at
                                                      -- the given number of seconds from the start of the video.
                                                      -- The parameter value is a positive integer

  youtubeTheme          :: !(Maybe YoutubeTheme)      -- ^ Dark or light theme.
                                                      -- default: Dark
} deriving (Show, Eq, Generic, Typeable, NFData)



defaultYoutubeEmbed :: YoutubeEmbed
defaultYoutubeEmbed = YoutubeEmbed {
  youtubeSrc            = VideoURL "https://www.youtube.com/watch?v = F6mHaUoNpOg",
  youtubeHeight         = Nothing,
  youtubeWidth          = Nothing,
  youtubeAutoPlay       = Nothing,
  youtubeCCLoadPolicy   = Nothing,
  youtubeColor          = Nothing,
  youtubeControls       = Nothing,
  youtubeDisableKb      = Nothing,
  youtubeEnableJsApi    = Nothing,
  youtubeEnd            = Nothing,
  youtubeFs             = Nothing,
  youtubeHl             = Nothing,
  youtubeIvLoadPolicy   = (),
  youtubeLoop           = Nothing,
  youtubeModestBranding = Nothing,
  youtubeOrigin         = Nothing,
  youtubePlaysInline    = Nothing,
  youtubeRel            = Nothing,
  youtubeShowInfo       = Nothing,
  youtubeStart          = Nothing,
  youtubeTheme          = Nothing
}



youtubeEmbedBaseURL :: Text
youtubeEmbedBaseURL = "https://youtube.com/embed"



youtubeEmbedToURL :: YoutubeEmbed -> Text
youtubeEmbedToURL YoutubeEmbed{..} =
  case qs of
    Nothing -> url <> "?" <> buildParams url_params
    Just qs' -> url <> qs' <> buildParams url_params
  where
  (url, qs) = case youtubeSrc of
                   VideoID video_id          -> (youtubeEmbedBaseURL <> "/" <> video_id, Nothing)
                   VideoURL video_url        -> (video_url, Nothing)
                   VideoPlaylist playlist_id -> (youtubeEmbedBaseURL, Just $ "listType=playlist&list=PL" <> playlist_id)
                   VideoUser user_id         -> (youtubeEmbedBaseURL, Just $ "listType=user_upload&list=" <> user_id)
                   VideoSearch search        -> (youtubeEmbedBaseURL, Just $ "listType=search&list=" <> search)
  url_params     = autoplay <> cc_load_policy <> color <> disable_kb
  autoplay       = maybeParam youtubeAutoPlay "autoplay"
  cc_load_policy = maybeParam youtubeCCLoadPolicy "cc_load_policy"
  color          = maybeParam youtubeColor "color"
  disable_kb     = maybeParam youtubeDisableKb "disablekb"
  -- youtubeControls       = Nothing,
  -- youtubeDisableKb      = Nothing,
  -- youtubeEnableJsApi    = Nothing,
  -- youtubeEnd            = Nothing,
  -- youtubeFs             = Nothing,
  -- youtubeHl             = Nothing,
  -- youtubeIvLoadPolicy   = (),
  -- youtubeLoop           = Nothing,
  -- youtubeModestBranding = Nothing,
  -- youtubeOrigin         = Nothing,
  -- youtubePlaysInline    = Nothing,
  -- youtubeRel            = Nothing,
  -- youtubeShowInfo       = Nothing,
  -- youtubeStart          = Nothing,
  -- youtubeTheme          = Nothing
