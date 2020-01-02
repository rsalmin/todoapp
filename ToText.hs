{-# LANGUAGE OverloadedStrings #-}
module ToText


where
import Data.Text (Text)
import qualified Data.Text as T

class ToText a where
    toText:: a -> Text

instance ToText Text where
    toText = id

instance (ToText a) => ToText [a] where
    toText xs = T.intercalate " " $ map toText xs

instance (ToText a) => ToText (Maybe a) where
    toText Nothing = "Nothing"
    toText (Just a) = T.append "Just " $ toText a
