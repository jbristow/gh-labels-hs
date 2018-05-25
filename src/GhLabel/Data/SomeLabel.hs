module GhLabel.Data.SomeLabel where

import qualified Data.Text          as T
import qualified GhLabel.Data.Label as L
import qualified GitHub.Data        as Github
import qualified GitHub.Data.Name   as Github
import           Text.Printf            (printf)
import Control.Arrow

data SomeLabel
  = GHL Github.IssueLabel
  | LBL L.Label
  | NULL ()
  deriving (Eq, Show)

showLabelPrintf :: (String, String) -> String
showLabelPrintf = uncurry (printf "%s:%s")

showLabel :: SomeLabel -> String -> String
showLabel (GHL lbl) _ =
  (((T.unpack . Github.untagName . Github.labelName) &&&
    (T.unpack . Github.labelColor)) >>>
   uncurry (printf "%s:%s"))
    lbl
showLabel (LBL lbl) _ =
  (((T.unpack . L.name) &&& (T.unpack . L.color)) >>> uncurry (printf "%s:%s"))
    lbl
showLabel _ x = x

class IsSomeLabel a where
  toSomeLabel :: a -> SomeLabel

instance IsSomeLabel L.Label where
  toSomeLabel = LBL

instance IsSomeLabel Github.IssueLabel where
  toSomeLabel = GHL

instance IsSomeLabel () where
  toSomeLabel = NULL

class HasLabelName a where
  toNameString :: a -> String

instance HasLabelName L.Label where
  toNameString = T.unpack . L.name

instance HasLabelName Github.IssueLabel where
  toNameString = T.unpack . Github.untagName . Github.labelName
