module Main where
import Data.List.Split
import Data.List

smallestArea :: [Int] -> Int
smallestArea l = head $ sort l

areaOfSides :: [Int] -> [Int]
areaOfSides [l,w,h] = [l*w , w*h , h*l]
areaOfSides _ = []

surfaceArea :: [Int] -> [Int]
surfaceArea l = map (*2) l

parseDimensions :: String -> [Int]
parseDimensions s = map read $ splitOn "x" s

totalNeededPaper :: [Int] -> Int
totalNeededPaper l = (smallestArea aos) + (sum $ surfaceArea aos)
 where aos = areaOfSides l

partOne :: String -> String
partOne input = show $ sum $ map (\a -> totalNeededPaper $ parseDimensions a) $ lines input


ribbonLengthForWrapping :: [Int] -> Int
ribbonLengthForWrapping l = a1 * 2 + a2 * 2
 where 
  sl = sort l
  a1 = head sl
  a2 = sl !! 1

ribbonLengthForBow :: [Int] -> Int
ribbonLengthForBow l = foldr (*) 1 l 

totalNeededRibbon :: [Int] -> Int
totalNeededRibbon l = (ribbonLengthForWrapping l) + (ribbonLengthForBow l)

partTwo :: String -> String
partTwo input = show $ sum $ map (\a -> totalNeededRibbon $ parseDimensions a) $ lines input

main :: IO ()
main = do
 input <- readFile "./input"
 putStrLn $ concat ["Part One output: ", partOne input]
 putStrLn $ concat ["Part Two output: ", partTwo input]
