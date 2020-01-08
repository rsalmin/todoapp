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

data Request = Empty | Add (Maybe Int) Text | Del [Int] | Shedule Int (Maybe UTCTime) (Maybe UTCTime)

instance PutText Request where
   putText Empty = putText ("Empty"::Text)
   putText (Add Nothing txt) = mapM_ putText ["Add ",  txt]
   putText (Add (Just n) txt)  = putText ("Add "::Text) >> putText  n >> putSpace >> putText  txt
   putText (Del ns) = putText ("Del "::Text) >> ( sequence_ $ intercalateM putSpace $ map putText ns )
   putText (Shedule n start stop) = sequence_ [putText ("Shedule "::Text), putText n, putText start, putText stop]


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

argsParser = anyOf [parseAddCmd, parseDelCmd]

parseArgs::Text -> Either  ParserError Request
parseArgs input =
    case (parse argsParser input) of
        Left err ->  Left err
        Right (res, _) -> return  res