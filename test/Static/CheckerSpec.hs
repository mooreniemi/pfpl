module Static.CheckerSpec where
import           Static.Checker
import           Test.Hspec

spec :: Spec
spec =
  describe "check'" $ do
    it "should return TNum for ENum expression" $
      check' (ENum 1) `shouldBe` Right TNum

    it "should return TStr for EStr expression" $
      check' (EStr "string") `shouldBe` Right TStr

    it "should return TNum for EAdd" $
       check' (EAdd (ENum 1) (ENum 1)) `shouldBe` Right TNum

    it "should return TNum for EMult" $
       check' (EMult (ENum 1) (ENum 1)) `shouldBe` Right TNum

    it "should return TStr for ECon" $
       check' (ECon (EStr "string") (EStr "string")) `shouldBe` Right TStr

    it "should return TStr for ELen" $
       check' (ELen (EStr "string")) `shouldBe` Right TNum

    it "should return TStr when e1 is EStr and x is e2" $
       check' (EDef (EStr "string") "x" (EId "x")) `shouldBe` Right TStr

    it "should return TNum when e1 is EStr and e2 is ELen of x" $
       check' (EDef (EStr "string") "x" (ELen (EId "x"))) `shouldBe` Right TNum

    it "should return helpful error messages for malformed expressions" $ do
       check' (EAdd (ENum 1) (EStr "1")) `shouldBe` Left  "EAdd (ENum 1) (EStr \"1\") expected EStr \"1\" to be TNum, but was TStr."

       check' (EMult (ENum 1) (EStr "1")) `shouldBe` Left  "EMult (ENum 1) (EStr \"1\") expected EStr \"1\" to be TNum, but was TStr."

       check' (ECon (ENum 1) (EStr "1")) `shouldBe` Left "ECon (ENum 1) (EStr \"1\") expected ENum 1 to be TStr, but was TNum."
 
