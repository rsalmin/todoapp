{-# LANGUAGE OverloadedStrings #-}
module CmdArgs


where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import PutText
import Data.Maybe
import Text.Read (readMaybe)

import Data.Time

data Request = Empty | Add (Maybe Int) Text | Del [Int] | Shedule Int (Maybe UTCTime) (Maybe UTCTime)

instance PutText Request where
   putText Empty = putText ("Empty"::Text)
   putText (Add Nothing txt) = mapM_ putText ["Add ",  txt]
   putText (Add (Just n) txt)  = putText ("Add "::Text) >> putText  n >> putSpace >> putText  txt
   putText (Del ns) = putText ("Del "::Text) >> ( sequence_ $ intercalateM putSpace $ map putText ns )
   putText (Shedule n start stop) = sequence_ [putText ("Shedule "::Text), putText n, putText start, putText stop]

data CmdError = EmptyInput | UnknownCommand Text | OtherError Text

instance PutText CmdError where
    putText EmptyInput = putText ("Input is Empty"::Text)
    putText (UnknownCommand txt) = mapM_ putText ["Unknown Command : ", txt]
    putText (OtherError txt) = mapM_ putText ["CmdError : ", txt]

type CmdErrorMonad = Either CmdError

parseAddCmd::[String] -> CmdErrorMonad Request
parseAddCmd [] = throwError $ OtherError "No description for Add command"
parseAddCmd lst@(x:xs) =
    case readMaybe x of
        Nothing -> return $ Add Nothing $ T.intercalate " " $ map T.pack lst
        Just n     -> return $ Add (Just n)  $ T.intercalate " " $ map T.pack xs


parseArgs::[String] -> CmdErrorMonad Request
parseArgs [] = return Empty
parseArgs (x:xs) =
   case x of
        ['a'] -> parseAddCmd xs
        ['d'] -> if null ints
                         then throwError $ OtherError "No valid id's  for Del command"
                         else return $ Del ints
        otherwise -> throwError $ UnknownCommand $ T.pack x
    where
        ints = catMaybes $ map readMaybe xs
