{-# LANGUAGE OverloadedStrings #-}
module CmdArgs


where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import ToText
import Data.Maybe
import Text.Read (readMaybe)

data Request = Empty | Add Text | Del [Int]

instance ToText Request where
   toText Empty = "Empty"
   toText (Add txt) = T.append "Add " txt
   toText (Del ns) = T.append "Del " $ T.intercalate " " $ map toText ns

data CmdError = EmptyInput | UnknownCommand Text | OtherError Text

instance ToText CmdError where
    toText EmptyInput = "Input is Empty"
    toText (UnknownCommand txt) = T.append "Unknown Command : " txt
    toText (OtherError txt) = T.append "CmdError : " txt

type CmdErrorMonad = Either CmdError


parseArgs::[String] -> CmdErrorMonad Request
parseArgs [] = throwError EmptyInput
parseArgs (x:xs) =
   case x of
        ['a'] -> if null xs
                         then throwError $ OtherError "No description for Add command"
                         else return $ Add $ T.intercalate " " $ map T.pack xs
        ['d'] -> if null ints
                         then throwError $ OtherError "No valid id's  for Del command"
                         else return $ Del ints
        otherwise -> throwError $ UnknownCommand $ T.pack x
    where
        ints = catMaybes $ map readMaybe xs
