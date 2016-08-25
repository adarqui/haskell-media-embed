{-# LANGUAGE OverloadedStrings #-}

module Web.Media.Embed.Internal (
    ToParam
  , toParam
  , maybeParam
  , buildParams
) where



import Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text



class ToParam a where
  toParam :: a -> Text



maybeParam :: ToParam a => Maybe a -> Text -> [Text]
maybeParam m_param param_name = maybe [] (\v -> [param_name <> "=" <> toParam v]) m_param



buildParams :: [Text] -> Text
buildParams ps = Text.intercalate "&" ps
