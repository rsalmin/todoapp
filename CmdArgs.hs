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

import Text.Parsec.Text
import Text.Parsec hiding (Empty)
import Text.Parsec.Error (errorMessages, messageString)

data Request = Empty | Add (Maybe Int) Text | Del [Int] | Shedule Int (Maybe UTCTime) (Maybe UTCTime)

instance PutText Request where
   putText Empty = putText ("Empty"::Text)
   putText (Add Nothing txt) = mapM_ putText ["Add ",  txt]
   putText (Add (Just n) txt)  = putText ("Add "::Text) >> putText  n >> putSpace >> putText  txt
   putText (Del ns) = putText ("Del "::Text) >> ( sequence_ $ intercalateM putSpace $ map putText ns )
   putText (Shedule n start stop) = sequence_ [putText ("Shedule "::Text), putText n, putText start, putText stop]

int::Parser Int
int = (read . T.pack ) <$> (many1 digit) <?> "parsing int"

optInt::Parser (Maybe Int)
optInt = option Nothing (Just <$> int)

rest::Parser Text
rest = T.pack <$> many1 anyToken

parseDelCmd = do
    char 'd'
    space
    spaces
    nums <-  int `sepBy1` spaces <?> "int list"
    return $ Del nums

parseAddCmd = do
    char 'a'
    space
    spaces
    taskId <- optInt
    spaces
    txt <- rest
    return $ Add taskId txt

argsParser = choice [parseAddCmd, parseDelCmd]

parseArgs::Text -> Either  Text Request
parseArgs input =
    case (parse argsParser "cmd parser" input) of
        Left err ->  Left $ T.pack $ show err
        Right res -> return  res