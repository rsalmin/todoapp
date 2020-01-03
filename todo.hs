{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, OverloadedLabels, DeriveDataTypeable,  OverloadedStrings, FlexibleInstances#-}

import BasePrelude hiding (on)
import Database.Selda hiding (toText)
import Database.Selda.PostgreSQL

import Control.Monad.Except

import CmdArgs
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import ToText

instance (ToText a, ToText b) => ToText ((:*:) a b) where
    toText ((:*:) a b) = T.concat [toText a, " :*: ", toText b]

data TodoEntry = TodoEntry { num :: ID TodoEntry, description :: Text }
    deriving Generic
instance SqlRow TodoEntry

instance ToText (ID TodoEntry) where
    toText  = toText . fromId
instance ToText TodoEntry where
    toText a = T.concat [toText $ num a, " : ", description a]

todoTable :: Table TodoEntry
todoTable = table "todo" [#num :- autoPrimary ]


withDB = withPostgreSQL ("todoplay" `on` "base.home" )

ensureTable = withDB $ tryCreateTable todoTable

queryTable =  withDB $ do
  todoList <- query $ do
    entry <- select todoTable
    return (entry ! #num :*: entry ! #description)

  liftIO $ TIO.putStrLn $ T.intercalate "\n" $ map toText todoList

--TODO check success
insertEntry dscr = withDB $ insert_ todoTable [ TodoEntry def dscr ]

delEntry ns = withDB $ forM_ ns $
    \n -> deleteFrom_ todoTable (\entry -> entry ! #num .== (literal $ toId n))

main :: IO ()
main = do
    args <- getArgs
    req <- case (parseArgs args) of
        Left err -> (TIO.putStrLn $ toText err) >> (return Empty)
        Right r -> return r
    ensureTable
    case req of
        Empty -> queryTable
        Add dscr -> insertEntry dscr
        Del  ns -> delEntry ns



