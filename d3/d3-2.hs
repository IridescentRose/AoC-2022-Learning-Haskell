{-# LANGUAGE LambdaCase #-}
import Data.List
import Data.Char

splitString :: [Char] -> (String, String)
splitString myList = splitAt ((length myList + 1) `div` 2) myList

splitList :: [String] -> [(String, String, String)]
splitList [] = []
splitList (s1:s2:s3:rest) = (s1, s2, s3) : splitList rest
splitList _ = error "List length is not a multiple of 3"

commonCharacters :: (String, String, String) -> Char
commonCharacters (t1, t2, t3) = head $ nub $ intersect t1 $ intersect t2 t3

convertCommon :: [(String, String, String)] -> [Char]
convertCommon = map commonCharacters

getCharacterValue :: Char -> Int
getCharacterValue c
    | isAsciiLower c    = ord c - ord 'a' + 1
    | isAsciiUpper c    = ord c - ord 'A' + 27
    | otherwise         = error "Invalid Characters!"


getCharacterSum :: [Char] -> Int
getCharacterSum = sum . map getCharacterValue

main :: IO ()
main = do
    content <- readFile "input"
    let input = lines content
    let commons = convertCommon $ splitList input
    let result = getCharacterSum commons
    print result