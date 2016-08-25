{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Media.Embed.YoutubeSpec (
    main
  , spec
) where



import           Web.Media.Embed
import           Data.Monoid          ((<>))
import qualified Data.Text            as Text
import           Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec = do



  -- describe "parseYoutube" $ do
  --   it "should parse a youtube source into VideoID, Playlist, Search, User etc" $ do
  --     (youtubeSrc $ parseYoutube "https://www.youtube.com/watch?v=gBzBn1wKWLY") `shouldBe` VideoID "gBzBn1wKWLY"
  --     (youtubeSrc $ parseYoutube "https://www.youtube.com/embed/gBzBn1wKWLY") `shouldBe` VideoID "gBzBn1wKWLY"
  --     (youtubeSrc $ parseYoutube "https://www.youtube.com/adarqui") `shouldBe` VideoUser "adarqui"

  describe "simpleYoutubeEmbedToIFrame" $ do
    it "should replace watch?v= with embad/" $ do
      (iframeSrc $ simpleYoutubeEmbedToIFrame "https://www.youtube.com/watch?v=gBzBn1wKWLY" defaultIFrame)
        `shouldBe` "https://www.youtube.com/embed/gBzBn1wKWLY"
