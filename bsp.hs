module Main where

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList (n-1); return (i:is)

displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess n grid  = 
  let fs = [head, last]
      dirs = case [getCol . getRow $ grid | getRow <- fs, getCol <- fs] of
        "p---" -> ["LEFT" , "UP"]
        "-p--" -> ["RIGHT", "UP"]
        "--p-" -> ["LEFT" , "DOWN"]
        "---p" -> ["RIGHT", "DOWN"]
  in init . unlines $ init . unlines . replicate (n `div` 2) <$> dirs

main = do
  n     <- getLine
  let i = read n
  grid  <- getList i
  putStrLn.displayPathtoPrincess i $ grid