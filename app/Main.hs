{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}

module Main where

import           Control.Exception
import           Data.Maybe             as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           GhLabel.Client
import qualified GhLabel.Data.Label     as L
import qualified GhLabel.Data.SomeLabel as SL
import           GitHub.Data            as Github
import           GitHub.Data.Name       as Github
import           System.Console.CmdArgs
import           Text.Printf            (printf)

{-# ANN Options ("HLint: ignore Use camelCase" :: String) #-}

data Options = Options
  { file     :: Text
  , token    :: Maybe Text
  , repo     :: Maybe Text
  , endpoint :: Text
  , owner    :: Maybe Text
  , dry_run  :: Bool
  } deriving (Show, Data, Typeable)

data OptionException
  = MissingOption String
  | MissingArg String
               Int

instance Show OptionException where
  showsPrec _ (MissingOption s) = showString ("missing required option: " ++ s)
  showsPrec _ (MissingArg s n) =
    showString
      ("option '" ++ s ++ "' requires at least " ++ show n ++ " argument(s).")

instance Exception OptionException

options :: Options
options =
  Options
  { file =
      "labels.yml" &= name "f" &=
      help "Paths to a YAML file containing the label template"
  , token = def &= help "OAuth token for authenticating with Github"
  , repo =
      def &= help "The name of the repository to apply the label template to"
  , endpoint = "https://api.github.com" &= help "GitHub API Endpoint"
  , owner = def &= help "help The name of the user or organization"
  , dry_run =
      False &= name "d" &=
      help "Print what the program would do without actually doing it."
  }

ownerN :: Options -> Github.Name Github.Owner
ownerN = Github.N . M.fromJust . owner

repoN :: Options -> Github.Name Github.Repo
repoN = Github.N . M.fromJust . repo

toAuth :: Options -> Github.Auth
toAuth opts =
  Github.EnterpriseOAuth
    (endpoint opts)
    (TE.encodeUtf8 (M.fromJust (token opts)))

maybeDo ::
     (SL.HasLabelName a, SL.IsSomeLabel a, SL.IsSomeLabel b, Exception e)
  => Options
  -> a
  -> String
  -> (Github.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> a -> IO (Either e b))
  -> IO String
maybeDo Options {dry_run = True, ..} label prefix _ =
  return
    (printf "DRY-RUN %s %s" prefix (SL.showLabel (SL.toSomeLabel label) ""))
maybeDo opts label prefix actionF = do
  eitherResult <- actionF (toAuth opts) (ownerN opts) (repoN opts) label
  return $
    case eitherResult of
      Left e ->
        printf
          "ERROR: %s %s: %s"
          prefix
          (SL.showLabel (SL.toSomeLabel label) "")
          (show e)
      Right result ->
        printf
          "%s %s"
          prefix
          (SL.showLabel (SL.toSomeLabel result) (SL.toNameString label))

maybeCreateLabel :: Options -> L.Label -> IO String
maybeCreateLabel opts label = maybeDo opts label "CREATE" createLabel

maybeUpdateLabel :: Options -> Github.IssueLabel -> IO String
maybeUpdateLabel opts label = maybeDo opts label "UPDATE" updateLabel

maybeDeleteLabel :: Options -> Github.IssueLabel -> IO String
maybeDeleteLabel opts label = maybeDo opts label "DELETE" deleteLabel

-- our main function
main :: IO ()
main = do
  opts <- validateOpts =<< cmdArgs options
  labels <- L.decodeLabelFile (T.unpack (file opts))
  eitherExistingLabels <- listGhLabels (toAuth opts) (ownerN opts) (repoN opts)
  case eitherExistingLabels of
    Left err -> throwIO err
    Right existingLabels -> do
      mapM_
        (fmap putStrLn . maybeCreateLabel opts)
        (L.newLabels existingLabels labels)
      mapM_
        (fmap putStrLn . maybeUpdateLabel opts)
        (L.updatedLabels existingLabels labels)
      mapM_
        (fmap putStrLn . maybeDeleteLabel opts)
        (L.deletedLabels existingLabels labels)

validateOpts :: Options -> IO Options
validateOpts Options {token = Nothing, ..} = throwIO $ MissingOption "token"
validateOpts Options {token = Just "", ..} = throwIO $ MissingArg "token" 1
validateOpts Options {repo = Nothing, ..}  = throwIO $ MissingOption "repo"
validateOpts Options {owner = Nothing, ..} = throwIO $ MissingOption "owner"
validateOpts opts                          = return opts
