module Main where

import Data.List ( unfoldr, elemIndices, sortBy )

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList (n-1); return (i:is)

split :: Eq a => a -> [a] -> [[a]]
split sep = takeWhile (not . null) . unfoldr (Just . span (/= sep) . dropWhile (== sep))

type Dim = (Int,Int)
type BotPos = (Int,Int)

nextMove :: BotPos -> Dim -> [String] -> String
nextMove bot dim board = case dirtyCells of
      [] -> "ENDOFGAME"
      _  -> calcMove . coords . fst . head $ dirtyCells
  where
    dirtyCells = sortBy (\(_, d1) (_, d2) -> compare d1 d2) $
                        (\c -> (c, distance . coords $ c)) <$> (elemIndices 'd' . concat $ board)
    calcMove (row, col)
      | posR < row = "DOWN"
      | posR > row = "UP"
      | posC < col = "RIGHT"
      | posC > col = "LEFT"
      | otherwise  = "CLEAN"
    distance (row, col) = abs (row - posR) + abs (col - posC)
    coords idx = (idx `div` dimC, idx `mod` dimC)
    (posR, posC) = bot
    (_   , dimC) = dim

main = do
   b <- getLine
   i <- getLine
   let bot = (read (head s)::Int, read (head (tail s))::Int) where s = split ' ' b
   let dim = (read (head s)::Int, read (head (tail s))::Int) where s = split ' ' i
   grid <- getList (fst dim)
   putStrLn $ nextMove bot dim grid