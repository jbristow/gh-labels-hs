{-# LANGUAGE OverloadedStrings #-}

module GhLabel.Data.SomeLabelSpec where

import qualified GhLabel.Data.Label as L
import qualified GhLabel.Data.SomeLabel as SL
import qualified GitHub.Data as Github
import qualified GitHub.Data.Name as Github
import Test.Hspec

main :: IO ()
main = hspec spec

testLabel :: L.Label
testLabel = L.Label {L.name = "basename", L.color = "basecolor"}

testGhLabel :: Github.IssueLabel
testGhLabel =
  Github.IssueLabel
  { Github.labelName = Github.N "ghname"
  , Github.labelColor = "ghcolor"
  , Github.labelUrl = Github.URL ""
  }

spec :: Spec
spec = do
  describe "#showLabelPrintf" $ do
    it "happy path example" $ SL.showLabelPrintf ("a", "b") `shouldBe` "a:b"
    it "empty strings" $ SL.showLabelPrintf ("", "") `shouldBe` ":"
  describe "#showLabel" $ do
    it "NULL type just shows default" $
      SL.showLabel (SL.NULL ()) "default" `shouldBe` "default"
    it "LBL type prints name and color" $
      SL.showLabel (SL.LBL testLabel) "default" `shouldBe` "basename:basecolor"
    it "GHL type prints name and color" $
      SL.showLabel (SL.GHL testGhLabel) "default" `shouldBe` "ghname:ghcolor"
  describe "#toSomeLabel" $ do
    it "converts to LBL" $ SL.toSomeLabel testLabel `shouldBe` SL.LBL testLabel
    it "converts to GHL" $
      SL.toSomeLabel testGhLabel `shouldBe` SL.GHL testGhLabel
    it "converts to NULL" $ SL.toSomeLabel () `shouldBe` SL.NULL ()
  describe "#toNameString" $ do
    it "gets the name of a Label" $
        SL.toNameString testLabel `shouldBe` "basename"
    it "gets the name of a Github.IssueLabel" $
        SL.toNameString testGhLabel `shouldBe` "ghname"
