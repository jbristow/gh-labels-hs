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
import           GitHub.Data            as Github
import           GitHub.Data.Name       as Github
import           System.Console.CmdArgs

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

class SomeLabel lbl where
  toLabelColor :: lbl -> String
  toLabelName :: lbl -> String

instance SomeLabel L.Label where
  toLabelColor = T.unpack . L.color
  toLabelName = T.unpack . L.name

instance SomeLabel Github.IssueLabel where
  toLabelColor = T.unpack . Github.labelColor
  toLabelName = T.unpack . Github.untagName . Github.labelName

maybeDo ::
     SomeLabel a
  => Options
  -> a
  -> (a -> IO b)
  -> (a -> String)
  -> (b -> String)
  -> IO String
maybeDo Options {dry_run = True, ..} label _ dryRunF _ = return (dryRunF label)
maybeDo _ label actionF _ logLineF = do
  result <- actionF label
  return (logLineF result)

defaultDryRunF :: SomeLabel a => String -> a -> String
defaultDryRunF = (("DRY-RUN " ++) .) . defaultLogLineF

defaultLogLineF :: SomeLabel a => String -> a -> String
defaultLogLineF prefix lbl =
  prefix ++ " " ++ toLabelName lbl ++ ":" ++ toLabelColor lbl

maybeCreateLabel :: Options -> L.Label -> IO String
maybeCreateLabel opts label =
  maybeDo
    opts
    label
    (createLabel (toAuth opts) (ownerN opts) (repoN opts))
    (defaultDryRunF "CREATE")
    (defaultLogLineF "CREATE")

maybeUpdateLabel :: Options -> Github.IssueLabel -> IO String
maybeUpdateLabel opts label =
  maybeDo
    opts
    label
    (updateLabel (toAuth opts) (ownerN opts) (repoN opts))
    (defaultDryRunF "UPDATE")
    (defaultLogLineF "UPDATE")

maybeDeleteLabel :: Options -> Github.IssueLabel -> IO String
maybeDeleteLabel opts label =
  maybeDo
    opts
    label
    (deleteLabel (toAuth opts) (ownerN opts) (repoN opts))
    (("DRY-RUN DELETE " ++) . toLabelName)
    (("DELETE " ++) . T.unpack)

toLogLine :: IO [String] -> IO ()
toLogLine = (putStr =<<) . fmap unlines

-- our main function
main :: IO ()
main = do
  opts <- validateOpts =<< cmdArgs options
  labels <- L.decodeLabelFile (T.unpack (file opts))
  existingLabels <- listGhLabels (toAuth opts) (ownerN opts) (repoN opts)
  toLogLine (mapM (maybeCreateLabel opts) (L.newLabels existingLabels labels))
  toLogLine
    (mapM (maybeUpdateLabel opts) (L.updatedLabels existingLabels labels))
  toLogLine
    (mapM (maybeDeleteLabel opts) (L.deletedLabels existingLabels labels))

validateOpts :: Options -> IO Options
validateOpts Options {token = Nothing, ..} = throwIO $ MissingOption "token"
validateOpts Options {token = Just "", ..} = throwIO $ MissingArg "token" 1
validateOpts Options {repo = Nothing, ..}  = throwIO $ MissingOption "repo"
validateOpts Options {owner = Nothing, ..} = throwIO $ MissingOption "owner"
validateOpts opts                          = return opts
