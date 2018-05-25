{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module GhLabel.Client where

import           Control.Exception
import qualified Data.Either                    as E
import           Data.List                      as L
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import qualified GhLabel.Data.Label             as L
import qualified GitHub.Auth                    as Github
import qualified GitHub.Data.Name               as Github
import qualified GitHub.Endpoints.Issues.Labels as Github

handleR :: Exception a => Either a b -> IO b
handleR = either throwIO return

listGhLabels ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> IO (Either Github.Error [Github.IssueLabel])
listGhLabels auth owner repo =
  fmap (fmap V.toList) (Github.labelsOnRepo' (Just auth) owner repo)

createLabel ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> L.Label
  -> IO (Either Github.Error Github.IssueLabel)
createLabel auth owner repo L.Label {name, color} =
  Github.createLabel auth owner repo (Github.N name) (T.unpack color)

updateLabel ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> Github.IssueLabel
  -> IO (Either Github.Error Github.IssueLabel)
updateLabel auth owner repo Github.IssueLabel { labelName = lname
                                              , labelColor = lcolor
                                              , ..
                                              } =
  Github.updateLabel auth owner repo lname lname (T.unpack lcolor)

deleteLabel ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> Github.IssueLabel
  -> IO (Either Github.Error ())
deleteLabel auth owner repo Github.IssueLabel {labelName = lname, ..} =
  Github.deleteLabel auth owner repo lname
