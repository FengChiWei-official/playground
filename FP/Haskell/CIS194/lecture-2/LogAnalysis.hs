{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


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
insert m@(LogMessage _ ts _) (Node l m'@(LogMessage _ ts' _) r) 
    | ts <= ts' = Node (insert m l) m' r 
    | otherwise = Node l m' (insert m r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build x = go Leaf x
    where 
        go :: MessageTree -> [LogMessage] -> MessageTree
        go acc [] = acc
        go acc (a:as) = go (insert a acc) as

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- 从排序后的LogMessage采集所有报错级别高于等于50的String信息
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getInfo $ filter tst x
    where
        tst (LogMessage (Error lv) _ _) = lv >= 50
        tst _ = False
        getInfo (LogMessage _ _ info) = info
        getInfo _ = "It is not for Unknown"