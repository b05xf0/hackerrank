module Main where
getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList (n-1); return (i:is)

displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess n grid  = case [getCol . getRow $ grid | getRow <- fs, getCol <- fs] of
  "p---" -> calcMoves ["LEFT" , "UP"]
  "-p--" -> calcMoves ["RIGHT", "UP"]
  "--p-" -> calcMoves ["LEFT" , "DOWN"]
  "---p" -> calcMoves ["RIGHT", "DOWN"]
  _      -> "INVALID INPUT"
  where
    calcMoves :: [String] -> String
    calcMoves dirs = init . unlines $ init . unlines . replicate (n `div` 2) <$> dirs
    fs = [head, last]

main = do
  n     <- getLine
  let i = read n
  grid  <- getList i
  putStrLn.displayPathtoPrincess i $ grid