{-# LANGUAGE OverloadedStrings #-}
{-|
  Two reasons for this module:
    a) Haskell won't have full support for unicode (f.e. deriving Show, printing exceptions, etc. and ever if you will
       use BasePrelude it didn't works properly - you may have still get wrong result)
    b) operations such as utcToLocalTimeZone (needed only at output) required IO monad
-}

module PutText


where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (intercalate)
import Data.Time
import Data.Time.LocalTime

class PutText a where
    putText :: a -> IO ()
    putTextLn :: a -> IO ()
    putTextLn a = putText a >> TIO.putStrLn ""

instance PutText Text where
    putText = TIO.putStr

instance  PutText Int where
    putText = TIO.putStr . T.pack . show

intercalateM:: m a -> [m a] -> [m a]
intercalateM _ [] = []
intercalateM _ [x] = [x]
intercalateM s (x:xs) = x:s:intercalateM s xs

putSpace = putText (" "::Text)

instance (PutText a) => PutText [a] where
    putText xs = sequence_ $ intercalateM putSpace $ map putText xs

instance (PutText a) => PutText (Maybe a) where
    putText Nothing = TIO.putStr "Nothing"
    putText (Just a) = TIO.putStr "Just " >> putText a

instance PutText UTCTime where
    putText t = do
         zt <- utcToLocalZonedTime t
         TIO.putStr $ T.pack $ formatTime defaultTimeLocale "%H:%M %d-%m-%y" t

instance PutText LocalTime where
    putText = TIO.putStr . T.pack . formatTime defaultTimeLocale "%H:%M %d-%m-%y"