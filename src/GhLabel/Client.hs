{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module GhLabel.Client where

import           Control.Exception
import           Control.Monad
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
  -> IO [Github.IssueLabel]
listGhLabels auth owner repo =
  either throwIO (return . V.toList) =<< Github.labelsOnRepo' (Just auth) owner repo

createLabel ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> L.Label
  -> IO Github.IssueLabel
createLabel auth owner repo L.Label {name, color} =
  handleR =<<
  Github.createLabel auth owner repo (Github.N name) (T.unpack color)

updateLabel ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> Github.IssueLabel
  -> IO Github.IssueLabel
updateLabel auth owner repo Github.IssueLabel { labelName = lname
                                              , labelColor = lcolor
                                              , ..
                                              } =
  handleR =<< Github.updateLabel auth owner repo lname lname (T.unpack lcolor)


deleteLabel ::
     Github.Auth
  -> Github.Name Github.Owner
  -> Github.Name Github.Repo
  -> Github.IssueLabel
  -> IO Text
deleteLabel auth owner repo Github.IssueLabel {labelName = lname, ..} =
  either throwIO (const (return (Github.untagName lname))) =<< Github.deleteLabel auth owner repo lname
