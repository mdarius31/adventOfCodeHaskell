module Main where

whatDir :: Char -> Int
whatDir '(' = 1
whatDir ')' = -1
whatDir _ = 0

partOne :: [Char] -> String
partOne input = show $ sum $ map whatDir input

partTwo :: [Char] -> String
partTwo input = case determinePos 0 0 $ map whatDir input of
  Just a -> show a
  Nothing -> "We never got to the basement"

determinePos :: Int -> Int -> [Int] -> Maybe Int
determinePos (-1) pos _ = Just pos
determinePos fl pos (d:ds) = determinePos (fl + d) (pos + 1) ds
determinePos _ _ _ = Nothing

main :: IO ()
main = do
  input <- readFile "./input"
  putStrLn $ concat ["Part One Output: ", partOne input]
  putStrLn $ concat ["Part Two Output: ", partTwo input]
