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
import Control.Applicative hiding (many)

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
    (\_ _  ns -> Del ns) <$> (char 'd') <*> spaces <*> ints

--parseAddCmd = do
--    char 'a'
--    space
--    spaces
--    taskId <- optInt
--    spaces
--    txt <- rest
--    return $ Add taskId txt

--argsParser = choice [parseAddCmd, parseDelCmd]

parseArgs::Text -> Either  Text Request
parseArgs input =
    case (parse parseDelCmd input) of
        Nothing ->  Left $ T.pack $ "Nothing"
        Just (res, _) -> return  res