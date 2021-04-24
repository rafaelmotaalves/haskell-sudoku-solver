module IO where
    
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readInput :: FilePath -> IO [[Int]]
readInput path = do l <- readLines path; return (map transformLine l)

transformLine :: String -> [Int]
transformLine = map read . split ' '

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs