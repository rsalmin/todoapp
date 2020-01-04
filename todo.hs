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

data TodoError = TodoError Text
    deriving Show

instance ToText TodoError where
    toText (TodoError txt) = T.append "Error: " txt

instance Exception TodoError



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


data ParentChildEntry = ParentChildEntry { parent :: ID TodoEntry, child :: ID TodoEntry }
    deriving Generic
instance SqlRow ParentChildEntry
instance ToText ParentChildEntry where
   toText p = T.concat [toText $ parent p, "->", toText $ child p]

pchTable :: Table ParentChildEntry
pchTable = table "parentchild" []

withDB = withPostgreSQL ("todoplay" `on` "base.home" )

ensureTables = withDB $ do
    tryCreateTable todoTable
    tryCreateTable pchTable

queryTable =  withDB $ do
  todoList <- query $ do
    entry <- select todoTable
    return (entry ! #num :*: entry ! #description)
  links <- query $ do
    entry <- select pchTable
    return (entry ! #parent :*: entry ! #child)

  liftIO $ TIO.putStrLn $ T.intercalate "\n" $ map toText todoList
  liftIO $ TIO.putStrLn "------------------------------------------------"
  liftIO $ TIO.putStrLn $ T.intercalate "\n" $ map toText links


checkPK pk = do
    nIDs <- query $ aggregate $ do
        entries <- select todoTable
        restrict ( entries ! #num .== literal pk)
        return (count $ entries ! #num)
    case nIDs of
        [1] -> return ()
        otherwise -> throw $ TodoError $ T.append "No task with ID " (toText pk)

--TODO check success
insertEntry prnt dscr = withDB $ do
    case prnt of
        Nothing -> insert_ todoTable [ TodoEntry def dscr ]
        Just n   -> do
                              checkPK (toId n)
                              newPK <- insertWithPK todoTable [ TodoEntry def dscr ]
                              insert_ pchTable [ ParentChildEntry (toId n) newPK ]


delEntry ns = withDB $ forM_ ns $
    \n -> deleteFrom_ todoTable (\entry -> entry ! #num .== (literal $ toId n))



seldaErrorHandler :: SeldaError -> IO ()
seldaErrorHandler  = print

todoErrorHandler :: TodoError -> IO ()
todoErrorHandler  = TIO.putStrLn . toText

main = main1 `catches` [Handler seldaErrorHandler, Handler todoErrorHandler]

main1 :: IO ()
main1 = do
    args <- getArgs
    req <- case (parseArgs args) of
        Left err -> (TIO.putStrLn $ toText err) >> (return Empty)
        Right r -> return r
    ensureTables
    case req of
        Empty -> return ()
        Add prnt dscr -> insertEntry prnt dscr
        Del  ns -> delEntry ns

    queryTable



