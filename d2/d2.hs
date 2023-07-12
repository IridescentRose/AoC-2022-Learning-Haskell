import System.IO

getPair :: IO (String, String)
getPair = do 
    end <- isEOF
    if end
        then return ("", "")
    else do 
        line <- getLine
        let [input1, input2] = words line
        return (input1, input2)

getAllPairs :: IO [(String, String)]
getAllPairs = do
    current <- getPair
    if current == ("", "")
        then return []
    else do
        rest <- getAllPairs
        return (current : rest)

getScoreWin :: (String, String) -> Int
getScoreWin tuple = case tuple of 
    (a, "X") -> case a of -- Rock
        "A" -> 3 -- Rock
        "B" -> 0 -- Paper
        "C" -> 6 -- Scissors
    (b, "Y") -> case b of -- Paper
        "A" -> 6 -- Rock
        "B" -> 3 -- Paper
        "C" -> 0 -- Scissors
    (c, "Z") -> case c of -- Scissors
        "A" -> 0 -- Rock
        "B" -> 6 -- Paper
        "C" -> 3 -- Scissors

getHandValue :: (String, String) -> Int
getHandValue tuple = case tuple of
    (_, "X") -> 1
    (_, "Y") -> 2
    (_, "Z") -> 3

getScore :: (String, String) -> Int
getScore = do
    win <- getScoreWin
    hand <- getHandValue
    return (win + hand)


getWinScore :: (String, String) ->Int
getWinScore tuple = case tuple of 
    (_, "X") -> 0
    (_, "Y") -> 3
    (_, "Z") -> 6

getHandScore :: (String, String) -> Int 
getHandScore tuple = case tuple of
    ("A", a) -> case a of -- Rock
        "X" -> 3 -- Scissors
        "Y" -> 1 -- Rock
        "Z" -> 2 -- Paper
    ("B", b) -> case b of -- Paper
        "X" -> 1 -- Rock
        "Y" -> 2 -- Paper
        "Z" -> 3 -- Scissors
    ("C", c) -> case c of -- Scissors
        "X" -> 2 -- Paper
        "Y" -> 3 -- Scissors
        "Z" -> 1 -- Rock

getScore2 :: (String, String) -> Int
getScore2 = do 
    win <- getWinScore
    hand <- getHandScore
    return (win + hand)


sumScore :: [(String, String)] -> Int
sumScore xs = sum (map getScore2 xs)

main :: IO ()
main = do 
    putStrLn "Pairs: "
    pairs <- getAllPairs
    let score = sumScore pairs
    print pairs
    putStrLn "Score: "
    print score