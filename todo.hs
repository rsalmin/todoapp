{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, OverloadedLabels, DeriveDataTypeable, TemplateHaskell#-}

import BasePrelude hiding (on)
import Database.Selda hiding (def, toText)
import Database.Selda.PostgreSQL

import CmdArgs
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import ToText

instance (ToText a, ToText b) => ToText ((:*:) a b) where
    toText ((:*:) a b) = T.concat [toText a, " :*: ", toText b]

data TodoEntry = TodoEntry { num :: Int, description :: Text }
    deriving Generic
instance SqlRow TodoEntry
instance ToText TodoEntry where
    toText a = T.concat [toText $ num a, " : ", description a]

todoTable :: Table TodoEntry
todoTable = table "todo" [#num :- primary]

main :: IO ()
main = withPostgreSQL ("todoplay" `on` "base.home" ) $ do
  tryCreateTable todoTable

  --insert_ todoTable
  --    [ TodoEntry 1 "Вымыть пол"
  --    , TodoEntry 2 "Всё убрать"
  --    , TodoEntry 3 "Сделать всё"
  --    ]

  todoList <- query $ do
    entry <- select todoTable
    return (entry ! #num :*: entry ! #description)

  liftIO $ TIO.putStrLn $ T.intercalate "\n" $ map toText todoList

