import System.IO
import Data.List

getNumLine :: IO Int
getNumLine = do 
    end <- isEOF
    if end
        then return 0
    else do
        s <- getLine
        return $ if null s then 0 else read s

sumOfLines :: IO Int
sumOfLines = do 
    num <- getNumLine
    if num == 0 
        then return 0
    else do
        partial_sum <- sumOfLines
        return (num + partial_sum)

sumList :: IO [Int]
sumList = do
    nextSum <- sumOfLines
    if nextSum == 0 then return []
    else do 
        rest <- sumList
        return (nextSum : rest)

main :: IO ()
main = do 
    list <- sumList
    let overall = maximum list
    let t3sum = sum (take 3 (reverse(sort list)))
    putStrLn "Top Overall: "
    print overall
    putStrLn "Top 3 Sum: "
    print t3sum