import Data.List
import Data.Char

splitString :: [Char] -> (String, String)
splitString myList = splitAt ((length myList + 1) `div` 2) myList

splitList :: [String] -> [(String, String)]
splitList = map splitString

commonCharacters :: (String, String) -> [Char]
commonCharacters (t1, t2) = nub $ intersect t1 t2

convertCommon :: [(String, String)] -> [[Char]]
convertCommon = map commonCharacters

getCharacterValue :: Char -> Int
getCharacterValue c
    | isAsciiLower c    = ord c - ord 'a' + 1
    | isAsciiUpper c    = ord c - ord 'A' + 27
    | otherwise         = error "Invalid Characters!"


getCharacterSum :: [[Char]] -> Int
getCharacterSum = sum . map (sum . map getCharacterValue)


main :: IO ()
main = do
    content <- readFile "input"
    let halfList = splitList $ lines content
    let common = convertCommon halfList
    let result = getCharacterSum common
    print result