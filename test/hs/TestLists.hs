module TestLists where

listInt :: [Int] -> [Int]
listInt = id

listDouble :: [Double] -> [Double]
listDouble = id

listChar :: [Char] -> [Char]
listChar = id

listString :: [String] -> [String]
listString = id

-- Nested

listListInt :: [[Int]] -> [[Int]]
listListInt = id

listListString :: [[String]] -> [[String]]
listListString = id

listListTupleStringString :: [[(String,String)]] -> [[(String, String)]]
listListTupleStringString = id

listListListString :: [[[String]]] -> [[[String]]]
listListListString = id

