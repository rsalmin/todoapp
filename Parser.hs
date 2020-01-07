{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Parser

where

import BasicPrelude
import Control.Monad.Except
import qualified Data.Text as T
import Data.Char

newtype Parser a = Parser {parse::Text -> Maybe (a, Text)}

instance Functor Parser where
   fmap f p = Parser $ \x ->
       case parse p x of
           Nothing -> Nothing
           Just (a, txt) -> Just (f a, txt)

instance Applicative Parser where
   pure v = Parser $ \x -> Just (v, x)
   p1 <*> p2 = Parser $ \x ->
       case parse p1 x of
           Nothing -> Nothing
           Just (f, x1) -> parse (f <$> p2) x1

parseFail = Parser $ \x->Nothing
parseAll  =  Parser $ \x->Just (x, "")

int::Parser Int
int = read . foldl1 T.append <$> (many (T.singleton <$> digit))

digit::Parser Char
digit = anyOf $ map (char . intToDigit) [0..9]

spaces::Parser [Char]
spaces = many space

space::Parser Char
space = anyOf [char ' ', char '\t']


sepBy::Parser a -> Parser b -> Parser [a]
sepBy p sep = Parser $ \x ->
    case parse p x of
        Nothing -> Nothing
        Just (a, b) -> case parse (many (sep *> p)) b of
                                  Nothing -> Just ([a], b)
                                  Just (as, rest) -> Just (a:as, rest)

many::Parser a -> Parser [a]
many p = Parser $ \x ->
   case parse p x of
      Nothing -> Nothing
      Just (a, b) -> case parse (many p) b of
                                 Nothing -> Just ([a], b)
                                 Just (as, rest) -> Just (a:as, rest)

anyOf::[Parser a] -> Parser a
anyOf [] = parseFail
anyOf (p:ps) = Parser $ \x ->
    case parse p x of
        Nothing -> parse (anyOf ps) x
        Just r -> Just r

char::Char -> Parser Char
char c = Parser $ \x ->
    let
        ch = T.head x
    in
        if T.null x
            then Nothing
            else
               if ch == c
                   then Just (ch, T.tail x)
                   else Nothing

word::Text -> Parser Text
word txt = Parser $ \x ->
   let
      (beg, rest) = T.splitAt (T.length txt) x
   in
    if beg == txt
        then  Just (beg, rest)
        else   Nothing

