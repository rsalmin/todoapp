{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, OverloadedLabels, DeriveDataTypeable,  OverloadedStrings, FlexibleInstances#-}

import BasicPrelude hiding (on)
import Database.Selda hiding (toText)
import Database.Selda.PostgreSQL

import Control.Exception
import Control.Monad.Except

import CmdArgs
import Print

import Data.Time
import Data.Time.LocalTime

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import PutText

data TodoError = TodoError (IO ())
instance Show TodoError where
    show e = "Todo Error Can't be shown outside of IO monad, but where it is?"

instance PutText TodoError where
    putText (TodoError m) = putText ("Error: "::Text) >> m

instance Exception TodoError



instance (PutText a, PutText b) => PutText ((:*:) a b) where
    putText ((:*:) a b) = sequence_ [putText a, putText (" :*: "::Text), putText b]



data TodoEntry = TodoEntry { num :: ID TodoEntry, description :: Text,
                                                    startShedule :: Maybe UTCTime, endShedule :: Maybe UTCTime}
    deriving Generic
instance SqlRow TodoEntry

instance PutText (ID TodoEntry) where
    putText  = putText . fromId
instance PutText TodoEntry where
    putText a = sequence_ [putText $ num a, putText (" : "::Text), putText $ description a]

todoTable :: Table TodoEntry
todoTable = table "todo" [#num :- autoPrimary ]


data ParentChildEntry = ParentChildEntry { parent :: ID TodoEntry, child :: ID TodoEntry }
    deriving Generic
instance SqlRow ParentChildEntry
instance PutText ParentChildEntry where
   putText p = sequence_ [putText $ parent p, putText ("->"::Text), putText $ child p]

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

  liftIO $ TIO.putStrLn $ showToDo todoList links

checkPK pk = do
    nIDs <- query $ aggregate $ do
        entries <- select todoTable
        restrict ( entries ! #num .== literal pk)
        return (count $ entries ! #num)
    case nIDs of
        [1] -> return ()
        otherwise -> throw $ TodoError $ putText ("No task with ID "::Text) >> (putText pk)

--TODO check success
insertEntry prnt dscr = withDB $ do
    case prnt of
        Nothing -> insert_ todoTable [ TodoEntry def dscr Nothing Nothing]
        Just n   -> do
                              checkPK (toId n)
                              newPK <- insertWithPK todoTable [ TodoEntry def dscr Nothing Nothing]
                              insert_ pchTable [ ParentChildEntry (toId n) newPK ]


delEntry ns = withDB $ forM_ ns $
    \n -> let nID = literal $ toId n in
             do
               deleteFrom_ todoTable (\entry -> entry ! #num .== nID)
               deleteFrom_ pchTable (\entry -> entry ! #parent .== nID .|| entry ! #child .== nID)

sheduleEntry tz n start stop = withDB $ do
    let startUTC = (localTimeToUTC tz) <$> start
    let stopUTC = (localTimeToUTC tz) <$> stop
    checkPK nID
    update todoTable (\entry-> entry ! #num .== literal nID)
        (\entry -> entry `with` [#startShedule := literal startUTC, #endShedule := literal stopUTC])
    liftIO $ TIO.putStrLn "Shedule not implemented yet"
    where
       nID = toId n

seldaErrorHandler :: SeldaError -> IO ()
seldaErrorHandler  = print

todoErrorHandler :: TodoError -> IO ()
todoErrorHandler  (TodoError m) = m >> TIO.putStrLn T.empty

main = main1 `catches` [Handler seldaErrorHandler, Handler todoErrorHandler]

main1 :: IO ()
main1 = do
    args <- getArgs
    tz <- getCurrentTimeZone
    lt <- liftM (utcToLocalTime tz) getCurrentTime
    req <- case (parseArgs lt $ T.intercalate " " args) of
        Left err -> TIO.putStr "command error:  " >> (putTextLn err) >> (return Empty)
        Right r -> (putTextLn r) >> return r
    ensureTables
    case req of
        Empty -> return ()
        Add prnt dscr -> insertEntry prnt dscr
        Del  ns -> delEntry ns
        Shedule idx ms me -> sheduleEntry tz idx ms me

    queryTable

