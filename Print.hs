{-# LANGUAGE  OverloadedStrings #-}
module Print
(
  showToDo
)
where

import Database.Selda hiding (toText)
import qualified Data.Text as T

-- expecting DAG
showToDo [] _ = ""
showToDo xs links = T.intercalate "\n" $ map (showTop xs links) $ filter (topTask links) xs

topTask [] _  = True
topTask ((prn :*: chld):rest) x@(idx :*: _ ) =
    if idx == chld
        then False
        else topTask rest x


showTop xs links x = T.intercalate "\n" $ showTop' xs links 0 x
showTop' xs links level (idx :*: dscr)  = tstr : concatMap (showTop' xs links (level + 1)) childs
    where
        childs = filter (\(xi :*: xd) -> elem (idx :*: xi) links) xs
        idxStr = show idx
        idxAlign = max 0 $ 4 - (length idxStr)
        spaces n = take n $ repeat ' '
        tstr = T.append (T.pack (idxStr ++ spaces (level*4 + idxAlign))) dscr
