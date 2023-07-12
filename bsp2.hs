module Main where

import Data.List ( elemIndex )

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList (n-1); return (i:is)

nextMove :: Int -> Int -> Int -> [String] -> String
nextMove n r c grid = case findPrincess of
  (Just pr, Just pc) -> calcMove pr pc
  _                  -> "INVALID INPUT"
  where
    findPrincess = (div <$> idx <*> pure n, mod <$> idx <*> pure n)
    idx = elemIndex 'p' . concat $ grid
    calcMove pr pc
      | pr > r = "DOWN"
      | pr < r = "UP"
      | pc > c = "RIGHT"
      | pc < c = "LEFT"

main :: IO ()
main = do
  n         <- getLine
  xy        <- getLine
  let i     = read n
      x:y:_ = read <$> words xy
  grid      <- getList i
  putStrLn . nextMove i x y $ grid