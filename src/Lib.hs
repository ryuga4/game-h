module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Queue a = Queue [a] [a]
             deriving (Show)



push :: a -> Queue a -> Queue a
push x (Queue [] []) = Queue [x] []
push x (Queue f b)   = Queue f $ x:b


pop :: Queue a -> Queue a

pop (Queue [] _)     = Queue [] []
pop (Queue [_] b)    = Queue (reverse b) []
pop (Queue (_:ft) b) = Queue ft b

front :: Queue a -> Maybe a

front (Queue [] _)    = Nothing
front (Queue (f:_) _) = Just f

popWhile :: (a -> Bool) -> Queue a -> Queue a
popWhile _ (Queue [] []) = Queue [] []
popWhile f q = case front q of
                 Nothing -> Queue [] []
                 Just x -> if f x
                              then popWhile f $ pop q
                              else q3
