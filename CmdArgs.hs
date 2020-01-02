{-# LANGUAGE OverloadedStrings #-}
module CmdArgs


where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import ToText

data Command = Empty | Add

instance ToText Command where
   toText Empty = "Empty"
   toText Add = "Add"

data Request = Request { cmd::Command, cmdArgs::[Text] }

instance ToText Request where
   toText a = T.append ( toText $ cmd a) $ toText (cmdArgs a)

data CmdError = EmptyInput | UnknownCommand Text | OtherError Text

instance ToText CmdError where
    toText EmptyInput = "Input is Empty"
    toText (UnknownCommand txt) = T.append "Unknown Command : " txt
    toText (OtherError txt) = T.append "CmdError : " txt

type CmdErrorMonad = Either CmdError


parseArgs::[String] -> CmdErrorMonad Request
parseArgs [] = throwError EmptyInput
parseArgs (x:xs) = parseCommand x >>=
   \cmd -> case cmd of
        Add -> if null xs
                         then throwError $ OtherError "No description for Add command"
                         else return $ Request Add $ map T.pack xs
        otherwise -> throwError $ OtherError "Command exists but unhandled... WTF?"


parseCommand::String -> CmdErrorMonad Command
parseCommand ['a'] = return Add
parseCommand xs = throwError $ UnknownCommand $ T.pack xs
