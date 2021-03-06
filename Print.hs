{-# LANGUAGE  OverloadedStrings #-}
module Print
(
  showToDo
)
where

import Database.Selda hiding (toText)
import qualified Data.Text as T

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock

-- expecting DAG
showToDo _ [] _ = ""
showToDo tz xs links = showTable $ concatMap (showTop tz xs links 0) $ filter (topTask links) xs

topTask [] _  = True
topTask ((prn :*: chld):rest) x@(idx :*: _ ) =
    if idx == chld
        then False
        else topTask rest x

spaces n = T.pack $ take n $ repeat ' '

alignLeft::Text -> Int -> Text
alignLeft str n = let len = T.length str in
    if len >= n
        then str
        else T.append str  (spaces (n - len))

showTable::[(Text, Text, Text)] -> Text
showTable inp = T.intercalate "\n" $ map showRow inp
    where
     showRow (t1, t2, t3) = T.concat [alignLeft t1 a1, alignLeft t2 a2, t3]
     (l1, l2, l3) = unzip3 inp
     a1 = 2 + (maximum $ map T.length l1)
     a2 = 2+ (maximum  $ map T.length  l2)

showTop tz xs links level (idx :*: dscr :*: start :*: stop)  =
    tstr : concatMap (showTop tz xs links (level + 1)) childs
    where
        childs = filter (\(xi :*: xd) -> elem (idx :*: xi) links) xs
        startLocal = utcToLocalTime tz <$> start
        stopLocal = utcToLocalTime tz <$> stop
        tstr = (T.pack $ show idx, showShedule startLocal stopLocal, T.append (spaces (4*level)) dscr)

showMLT::Maybe LocalTime -> Text
showMLT Nothing = ""
showMLT (Just lt) = T.pack $ formatTime defaultTimeLocale "day %d.%m.%Y" lt

showMLTT::Maybe LocalTime -> Text
showMLTT Nothing = ""
showMLTT (Just lt) = T.pack $ formatTime defaultTimeLocale "%H:%M %d.%m.%Y" lt

showT::LocalTime -> Text
showT lt = T.pack $ formatTime defaultTimeLocale "%H:%M" lt

showD::LocalTime -> Text
showD lt = T.pack $ formatTime defaultTimeLocale "%d.%m.%Y" lt


showShedule::Maybe LocalTime -> Maybe LocalTime -> Text
showShedule start stop =
    case (start, stop) of
        (Nothing, Nothing) -> ""
        (Just s, Nothing)      -> T.concat [showMLTT start, " -  till end"]
        (Nothing, Just s)      -> T.concat ["from start - ", showMLTT stop]
        (Just s, Just e)          -> showSheduleInterval s e

showSheduleInterval::LocalTime -> LocalTime -> Text
showSheduleInterval s e =
    if oneDay s e
        then showMLT (Just s)
        else if (localDay s) == (localDay e)
                    then T.concat [showT s, "-", showMLTT (Just e)]
                    else T.concat [showMLTT (Just s), " - ", showMLTT (Just e)]

--Check if shedule time is exactly one day from 00:00
oneDay::LocalTime -> LocalTime -> Bool
oneDay start stop = (diffLocalTime stop start == nominalDay) && (localTimeOfDay start == midnight)
