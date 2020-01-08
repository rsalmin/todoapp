{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module CmdArgs


where
import BasicPrelude
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import PutText
import Data.Maybe
import Text.Read (readMaybe)

import Data.Time

import Parser
import Control.Applicative hiding (many, optional)

data Request = Empty | Add (Maybe Int) Text | Del [Int] | Shedule Int (Maybe LocalTime) (Maybe LocalTime)

instance PutText Request where
   putText Empty = putText ("Empty"::Text)
   putText (Add Nothing txt) = mapM_ putText ["Add ",  txt]
   putText (Add (Just n) txt)  = putText ("Add "::Text) >> putText  n >> putSpace >> putText  txt
   putText (Del ns) = putText ("Del "::Text) >> ( sequence_ $ intercalateM putSpace $ map putText ns )
   putText (Shedule n start stop) = sequence_ [putText ("Shedule "::Text), putText n, putSpace, putText start, putSpace, putText stop]


ints::Parser [Int]
ints = sepBy int  spaces

parseDelCmd::Parser Request
parseDelCmd  =
    (\_ _  ns _ -> Del ns)
       <$> (char 'd')  <?> "Expecting command d (Del)"
       <*> spaces <?> "Command Del expected List of Ints"
       <*> ints      <?> "Command Del Expected List of Ints"
       <*> eof       <?> "Not Ints at the end of input"

parseAddCmd =
   (\_ _ idx _ txt -> Add idx txt)
       <$> (char 'a') <?> "Expecting command a (Add)"
       <*> spaces  <?> "Missing arguments to Add command"
       <*> optional (int <* spaces)
       <*> (checknot digit) <?> "Task description cannot starts with digit"
       <*> parseAll

parseSheduleCmd lt = do
       (char 's') <?> "Expecting command s (Shedule)"
       spaces  <?> "Missing arguments to Shedule command"
       idx <-  int <?> "Expecting task index"
       spaces <?> "Missing time/date arguments from Shedule command"
       d <- day lt <?> "Expecting date"
       return $ Shedule idx (Just d) Nothing

argsParser lt = anyOf [parseAddCmd, parseDelCmd, parseSheduleCmd lt]

parseArgs::LocalTime -> Text -> Either  ParserError Request
parseArgs lt input =
    case (parse (argsParser lt) input) of
        Left err ->  Left err
        Right (res, _) -> return  res