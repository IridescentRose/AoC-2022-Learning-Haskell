import Data.List.Split
import Data.List

splitLines :: String -> [String]
splitLines = lines

tuple2 :: [a] -> (a, a)
tuple2 [x, y] = (x, y)

splitTuple :: String -> (String, String)
splitTuple = tuple2 <$> splitOn ","

splitTuples :: [String] -> [(String, String)]
splitTuples = map splitTuple

splitRange :: String -> (Int, Int)
splitRange = tuple2 <$> map read . splitOn "-"

findRangeContains :: (String, String) -> Bool
findRangeContains (x, y) = do
    let (x1, x2) = splitRange x -- Range 1
    let (y1, y2) = splitRange y -- Range 2
    let x1y1 = x1 <= y1 && y2 <= x2 -- Check if range 1 contains range 2
    let y1x1 = y1 <= x1 && x2 <= y2 -- Check if range 2 contains range 1
    x1y1 || y1x1

countFullyContained :: [(String, String)] -> Int
countFullyContained = length . filter findRangeContains

findAnyOverlap :: (String, String) -> Bool
findAnyOverlap (x, y) = do
    let (x1, x2) = splitRange x -- Range 1
    let (y1, y2) = splitRange y -- Range 2
    let xRange = [x1 .. x2]
    let yRange = [y1 .. y2]
    let overlap = xRange `intersect` yRange
    not(null overlap)

countOverlap :: [(String, String)] -> Int
countOverlap = length . filter findAnyOverlap

main :: IO ()
main = do 
    content <- readFile "input"
    let tuples = splitTuples $ splitLines content
    let count = countFullyContained tuples
    let count2 = countOverlap tuples
    print count
    print count2