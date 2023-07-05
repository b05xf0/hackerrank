module Main where

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList (n-1); return (i:is)

displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess n grid  = 
  let selectors = [head, last]
      dirs = case [selectCol . selectRow $ grid | selectRow <- selectors, selectCol <- selectors] of
        'p':_       -> ["LEFT" , "UP"]
        _:'p':_     -> ["RIGHT", "UP"]
        _:_:'p':_   -> ["LEFT" , "DOWN"]
        _:_:_:'p':_ -> ["RIGHT", "DOWN"]
        _           -> []
  in if   null dirs 
     then "INVALID INPUT" 
     else init . unlines $ init . unlines . replicate (n `div` 2) <$> dirs

main = do
  n     <- getLine
  let i = read n
  grid  <- getList i
  putStrLn.displayPathtoPrincess i $ grid