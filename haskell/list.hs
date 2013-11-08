joinBy :: [a] -> [[a]] -> [a]
joinBy _ [] = []
joinBy _ [a] = a
joinBy sep (a:as) = a ++ sep ++ joinBy sep as
