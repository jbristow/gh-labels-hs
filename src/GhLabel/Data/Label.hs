{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module GhLabel.Data.Label where

import           Control.Exception
import           Data.Aeson              (FromJSON (..), ToJSON (..), object,
                                          (.:), (.=))
import qualified Data.List               as L
import qualified Data.Maybe              as M
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import qualified Data.Yaml               as YML
import           GHC.Generics
import qualified GitHub.Data.Definitions as Github
import qualified GitHub.Data.Name        as Github
import qualified GitHub.Data.URL         as Github

data Label = Label
  { labelName  :: Text
  , labelColor :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Label where
  parseJSON (YML.Object v) = Label <$> v .: "name" <*> v .: "color"
  parseJSON _              = fail "Expected Object for Label value"

instance ToJSON Label where
  toJSON Label {labelName, labelColor} =
    object ["name" .= labelName, "color" .= labelColor]

decodeLabelFile :: FilePath -> IO [Label]
decodeLabelFile f = do
  lsEither <- YML.decodeFileEither f
  case lsEither of
    Left e       -> throwIO e
    Right labels -> return labels

toIssueLabel :: Label -> Github.IssueLabel
toIssueLabel x =
  Github.IssueLabel
  { Github.labelColor = labelColor x
  , Github.labelName = Github.N $ labelName x
  , Github.labelUrl = Github.URL ""
  }

labelNameEq :: Label -> Github.IssueLabel -> Bool
labelNameEq x y = labelName x == Github.untagName (Github.labelName y)

ghLabelExistsIn :: [Github.IssueLabel] -> Label -> Maybe Label
ghLabelExistsIn xs y =
  maybe (Just y) (const Nothing) (L.find (labelNameEq y) xs)

newLabels :: [Github.IssueLabel] -> [Label] -> [Label]
newLabels = M.mapMaybe . ghLabelExistsIn

ghLabelNameEq :: Github.IssueLabel -> Github.IssueLabel -> Bool
ghLabelNameEq x y =
  Github.untagName (Github.labelName x) == Github.untagName (Github.labelName y)

hasChanged :: Github.IssueLabel -> Label -> Bool
hasChanged x y = labelNameEq y x && Github.labelColor x /= labelColor y

updateIssueLabel :: Github.IssueLabel -> Label -> Maybe Github.IssueLabel
updateIssueLabel x y = Just $ x {Github.labelColor = labelColor y}

updatedLabel :: [Label] -> Github.IssueLabel -> Maybe Github.IssueLabel
updatedLabel ls x =
  maybe Nothing (updateIssueLabel x) (L.find (hasChanged x) ls)

updatedLabels :: [Github.IssueLabel] -> [Label] -> [Github.IssueLabel]
updatedLabels = flip (M.mapMaybe . updatedLabel)

deletedLabels :: [Github.IssueLabel] -> [Label] -> [Github.IssueLabel]
deletedLabels x y = L.deleteFirstsBy ghLabelNameEq x (map toIssueLabel y)
