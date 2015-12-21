module Static.CheckerSpec where
import           Static.Checker
import           Test.Hspec

spec :: Spec
spec =
  describe "check'" $ do
    it "should return TNum for ENum expression" $
      check' (ENum 1) `shouldBe` TNum

    it "should return TStr for EStr expression" $
      check' (EStr "string") `shouldBe` TStr

    it "should return TNum for EAdd" $
       check' (EAdd (ENum 1) (ENum 1)) `shouldBe` TNum

    it "should return TNum for EMult" $
       check' (EMult (ENum 1) (ENum 1)) `shouldBe` TNum

    it "should return TStr for ECon" $
       check' (ECon (EStr "string") (EStr "string")) `shouldBe` TStr

    it "should return TStr for ELen" $
       check' (ELen (EStr "string")) `shouldBe` TNum

    it "should return TStr when e1 is EStr and x is e2" $
       check' (EDef (EStr "string") "x" (EId "x")) `shouldBe` TStr

    it "should return TNum when e1 is EStr and e2 is ELen of x" $
       check' (EDef (EStr "string") "x" (ELen (EId "x"))) `shouldBe` TNum
