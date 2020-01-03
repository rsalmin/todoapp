{-# LANGUAGE OverloadedStrings #-}
module CmdArgs


where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import ToText
import Data.Maybe
import Text.Read (readMaybe)

data Request = Empty | Add (Maybe Int) Text | Del [Int]

instance ToText Request where
   toText Empty = "Empty"
   toText (Add Nothing txt) = T.append "Add " txt
   toText (Add (Just n) txt)  = T.concat ["Add ", toText n, " ", txt]
   toText (Del ns) = T.append "Del " $ T.intercalate " " $ map toText ns

data CmdError = EmptyInput | UnknownCommand Text | OtherError Text

instance ToText CmdError where
    toText EmptyInput = "Input is Empty"
    toText (UnknownCommand txt) = T.append "Unknown Command : " txt
    toText (OtherError txt) = T.append "CmdError : " txt

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
