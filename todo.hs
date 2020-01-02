{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, OverloadedLabels, DeriveDataTypeable, TemplateHaskell#-}

import BasePrelude hiding (on)
import Database.Selda hiding (def, toText)
import Database.Selda.PostgreSQL

import CmdArgs
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import ToText

instance (ToText a, ToText b) => ToText ((:*:) a b) where
    toText ((:*:) a b) = T.concat [toText a, " :*: ", toText b]

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet
instance ToText Pet where
    toText = T.pack . show

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people1" [#name :- primary]

main :: IO ()
main = withPostgreSQL ("todoplay" `on` "base.home" ) $ do
  --createTable people
  --insert_ people
  --  [ Person "Петя"    19 (Just Dog)
  --  , Person "Женя" 23 (Just Dragon)
  --  , Person "Света"      10 Nothing
  --  ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name :*: person ! #pet)
  liftIO $ TIO.putStrLn $ toText adultsAndTheirPets
