{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Parser

where

import BasicPrelude
import Control.Monad.Except
import qualified Data.Text as T
import Data.Char
import PutText

import Data.Time
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar

import Control.Monad.Fail

data ParserError = EmptyInput Text | ParserError Text
    deriving Show


newtype Parser a = Parser {parse::Text -> Either ParserError (a, Text)}

instance PutText ParserError where
    putText (EmptyInput txt) = putText $ T.append "Empty input. "  txt
    putText (ParserError txt) = putText txt

instance Functor Parser where
   fmap f p = Parser $ \x ->
       case parse p x of
           Left err -> Left err
           Right (a, txt) -> Right $ first f (a, txt)

instance Applicative Parser where
   pure v = Parser $ \x -> Right (v, x)
   p1 <*> p2 = Parser $ \x ->
       case parse p1 x of
           Left err -> Left err
           Right (f, x1) -> parse (f <$> p2) x1

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \x ->
        case parse p x of
            Left err -> Left err
            Right (a, xs) -> parse (f a) xs

instance MonadFail Parser where
    fail s = Parser $ \x -> Left $ ParserError $ T.pack s

(<??>)::Parser a -> (ParserError -> ParserError)-> Parser a
(<??>) p errf = Parser $ \x ->
    case parse p x of
        Left err -> Left $ errf err
        Right r -> Right r

(<?>) p txt = p <??> const (ParserError txt)

parseAll  =  Parser $ \x->Right (x, "")

int::Parser Int
int = (read . foldl1 T.append <$> (many (T.singleton <$> digit))) <?> "Expecting integer"

digit::Parser Char
digit = anyOf $ map (char . intToDigit) [0..9]

spaces::Parser [Char]
spaces = many space

space::Parser Char
space = anyOf [char ' ', char '\t']


optional::Parser a -> Parser (Maybe a)
optional p = Parser $ \x ->
    case parse p x of
       Left err -> Right (Nothing, x)
       Right (a, b) -> Right (Just a, b)

withDef::a -> Parser a -> Parser a
withDef d p = Parser $ \x ->
    case parse p x of
        Left _ -> Right (d, x)
        Right (a, b) -> Right (a, b)

sepBy::Parser a -> Parser b -> Parser [a]
sepBy p sep = Parser $ \x ->
    case parse p x of
        Left err -> Left err
        Right (a, b) -> case parse (many (sep *> p)) b of
                                  Left _ -> Right ([a], b)
                                  Right (as, rest) -> Right (a:as, rest)

many::Parser a -> Parser [a]
many p = Parser $ \x ->
   case parse p x of
      Left err -> Left err
      Right (a, b) -> case parse (many p) b of
                                 Left _ -> Right ([a], b)
                                 Right (as, rest) -> Right (a:as, rest)


appendif::ParserError->ParserError->ParserError
appendif err txt =
    case (err, txt) of
        (EmptyInput e, EmptyInput t) -> EmptyInput $ T.intercalate " or " [t, e]
        (EmptyInput e, ParserError t) -> txt
        (ParserError e, EmptyInput t) -> err
        (ParserError e, ParserError t) -> ParserError $ T.intercalate " or " [t, e]

anyOf::[Parser a] -> Parser a
anyOf [] = Parser $ \x -> Left $ EmptyInput ""
anyOf (p:ps) = Parser $ \x ->
    case parse p x of
        Left err -> parse (anyOf ps <??>appendif err) x
        Right r -> Right r


eof::Parser ()
eof = Parser $ \x -> if T.null x then Right ((), "") else Left $ ParserError "Expecting EOF"

checknot::Parser a -> Parser ()
checknot p = Parser $ \x->
   case parse p x of
       Left _ -> Right ((), x)
       Right _ -> Left $ ParserError "checknot failed"


char::Char -> Parser Char
char c = Parser $ \x ->
    let
        ch = T.head x
    in
        if T.null x
            then Left $ EmptyInput $ "expecting char " ++ tshow c
            else
               if ch == c
                   then Right (ch, T.tail x)
                   else Left $ ParserError $ "expecting char " ++ tshow c

word::Text -> Parser Text
word txt = Parser $ \x ->
   let
      (beg, rest) = T.splitAt (T.length txt) x
   in
    if T.null x
       then Left $ EmptyInput $ "expecting word " ++ txt
       else if beg == txt
           then  Right (beg, rest)
           else   Left $ ParserError $ "expecting word " ++ txt




--TODO: Fix to Either monad
day::LocalTime -> Parser LocalTime
day defTime = dayStr defTime >>= (parseTimeM True defaultTimeLocale "%d %m %Y")


intString = many digit
show0 x = let s = show x in (if length s == 1 then "0" else "") ++ s

dayStr::LocalTime -> Parser String
dayStr now = (\d m y -> intercalate " " [d, m, y])
     <$> intString
     <*> ( withDef (show0 defM) (char '.' *> intString) )
     <*> ( withDef (show defY)  (char '.' *> intString) )
    where
      (defY, defM, defD) = toGregorian $ localDay now
