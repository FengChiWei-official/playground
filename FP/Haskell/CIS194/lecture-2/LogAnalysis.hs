{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Maybe (mapMaybe)


parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Null Message!"
parseMessage s = case words s of
  ("I":ts:rest) -> LogMessage Info (read ts) (unwords rest)
  ("W":ts:rest) -> LogMessage Warning (read ts) (unwords rest)
  ("E":level:ts:rest) -> LogMessage (Error (read level)) (read ts) (unwords rest)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m (Node l m'@(LogMessage _ ts' _) r) =
    case m of
        LogMessage _ ts _
            | ts <= ts' -> Node (insert m l) m' r
            | otherwise -> Node l m' (insert m r)
insert _ node@(Node _ (Unknown _) _) = node

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapMaybe severeInfo . inOrder . build
    where
        severeInfo (LogMessage (Error lv) _ info)
            | lv >= 50 = Just info
        severeInfo _ = Nothing