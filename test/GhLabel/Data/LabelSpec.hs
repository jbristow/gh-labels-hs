{-# LANGUAGE OverloadedStrings #-}

module GhLabel.Data.LabelSpec where

import qualified Data.Maybe              as M
import qualified Data.Yaml               as YML
import qualified GhLabel.Data.Label      as L
import qualified GitHub.Data.Definitions as Github
import qualified GitHub.Data.Name        as Github
import qualified GitHub.Data.URL         as Github
import           Test.Hspec

main :: IO ()
main = hspec spec

testIssueLabel :: Github.IssueLabel
testIssueLabel =
  Github.IssueLabel
  { Github.labelName = Github.N "test-name"
  , Github.labelColor = "eeff00"
  , Github.labelUrl = Github.URL ""
  }

testLabel :: L.Label
testLabel = L.Label {L.name = "test-name", L.color = "eeff00"}

testLabelB :: L.Label
testLabelB = L.Label {L.name = "test-name-2", L.color = "123456"}

spec :: Spec
spec = do
  describe "Data.Yaml#decode :: Label" $ do
    it "simple yaml decodes successfully" $
      YML.decode "name: test-name\ncolor: eeff00" `shouldBe` Just testLabel
    it "simple quoted yaml decodes successfully" $
      YML.decode "name: test-name\ncolor: \"123456\"" `shouldBe`
      Just testLabel {L.color = "123456"}
  describe "#toIssueLabel" $
    it "converts to a simple Github issueLabel nicely" $
    L.toIssueLabel testLabel `shouldBe` testIssueLabel
  describe "#labelNameEq" $ do
    it "simple equality example" $
      testIssueLabel `shouldSatisfy` L.labelNameEq testLabel
    it "simple inequality example" $
      testIssueLabel `shouldNotSatisfy`
      L.labelNameEq testLabel {L.name = "other"}
  describe "#noGhLabelExistsIn" $ do
    it "handle empty array gracefully" $
      L.noGhLabelExistsIn [] testLabel `shouldBe` Just testLabel
    it "handles single match" $
      L.noGhLabelExistsIn [testIssueLabel] testLabel `shouldSatisfy` M.isNothing
    it "handles match coming second" $
      L.noGhLabelExistsIn
        [ testIssueLabel {Github.labelName = Github.N "test-name-other"}
        , testIssueLabel
        ]
        testLabel `shouldSatisfy`
      M.isNothing
    it "handles single match with different color" $
      L.noGhLabelExistsIn
        [testIssueLabel {Github.labelColor = "123456"}]
        testLabel `shouldSatisfy`
      M.isNothing
  describe "#newLabels" $ do
    it "handle empty arrays gracefully" $ L.newLabels [] [] `shouldBe` []
    it "handles one preexisting label" $
      L.newLabels [testIssueLabel] [testLabel] `shouldBe` []
    it "handles all new" $
      L.newLabels [] [testLabel, testLabelB] `shouldBe` [testLabel, testLabelB]
    it "handles one new" $
      L.newLabels [testIssueLabel] [testLabelB, testLabel] `shouldBe`
      [testLabelB]
  describe "#ghLabelNameEq" $ do
    it "identity" $
      testIssueLabel `shouldSatisfy` L.ghLabelNameEq testIssueLabel
    it "different color, same name" $
      testIssueLabel `shouldSatisfy`
      L.ghLabelNameEq testIssueLabel {Github.labelColor = "ffffff"}
    it "unequal name" $
      testIssueLabel `shouldNotSatisfy`
      L.ghLabelNameEq
        testIssueLabel {Github.labelName = Github.N "test-name-other"}
  describe "#hasChanged" $ do
    it "same name same color returns false" $
        testLabel `shouldNotSatisfy` L.hasChanged testIssueLabel
    it "different name same color returns false" $
        testLabel {L.name = "different-name"} `shouldNotSatisfy` L.hasChanged testIssueLabel
    it "different name different color returns false" $
        testLabelB `shouldNotSatisfy` L.hasChanged testIssueLabel
    it "same name different color returns false" $
        testLabel {L.color = "ffffff"} `shouldSatisfy` L.hasChanged testIssueLabel
