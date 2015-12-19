module Static.CheckerSpec where
import           Static.Checker
import           Test.Hspec
                 
spec :: Spec
spec = do
  describe "check" $ do
    it "should return TNum for ENum expression" $ do
      check (ENum 1) `shouldBe` TNum





