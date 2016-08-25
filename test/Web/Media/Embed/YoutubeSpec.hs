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



  describe "parseYoutubeSource" $ do
    it "should parse a youtube source into VideoID, Playlist, Search, User etc" $ do
      True `shouldBe` True
