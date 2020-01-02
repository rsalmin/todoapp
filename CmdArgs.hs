{-# LANGUAGE OverloadedStrings #-}
module CmdArgs


where
import Data.Text (Text)
import qualified Data.Text as T

import ToText

data Command = Add | Unknown Text

instance ToText Command where
   toText Add = "Add"
   toText (Unknown txt) = T.append "Unknown: "  txt

data Args = Args { cmd::Command, args::[Text] }

instance ToText Args where
   toText a = T.append ( toText $ cmd a) $ toText (args a)

parseArgs::[String] -> Args
parseArgs [] = Args (Unknown "") []
parseArgs (x:xs) = Args (parseCommand x) (map T.pack xs)

parseCommand::String -> Command
parseCommand ['a'] = Add
parseCommand xs = Unknown $ T.pack xs
