module Main where

import Data.List ( elemIndices, sortBy )

getList :: Int -> IO [String]
getList n = if n == 0 then return [] else do i <- getLine; is <- getList (n - 1); return (i:is)

nextMove :: String -> [String] -> String
nextMove pos board = case dirtyCells of
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
    coords idx = (idx `div` 5, idx `mod` 5)
    posR:posC:_ = read <$> words pos

main = do
    pos   <- getLine
    board <- getList 5
    putStrLn $ nextMove pos board