{-# LANGUAGE OverloadedStrings #-}

module GhLabel.Data.LabelSpec where

import qualified Data.Yaml               as YML
import qualified GhLabel.Data.Label      as L
import qualified GitHub.Data.Definitions as Github
import qualified GitHub.Data.Name        as Github
import qualified GitHub.Data.URL         as Github
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.Yaml#decode :: Label" $ do
    it "simple yaml decodes successfully" $
      YML.decode "name: test-name\ncolor: eeff00" `shouldBe`
      Just L.Label {L.labelName = "test-name", L.labelColor = "eeff00"}
    it "simple quoted yaml decodes successfully" $
      YML.decode "name: test-name\ncolor: \"123456\"" `shouldBe`
      Just L.Label {L.labelName = "test-name", L.labelColor = "123456"}
  describe "GhLabel.Data.Label#toIssueLabel" $
    it "converts to a simple Github issueLabel nicely" $
    L.toIssueLabel L.Label {L.labelName = "test-name", L.labelColor = "123456"} `shouldBe`
    Github.IssueLabel
    { Github.labelName = Github.N "test-name"
    , Github.labelColor = "123456"
    , Github.labelUrl = Github.URL ""
    }

