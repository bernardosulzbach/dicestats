module Generator (generateRolls) where

prependOne :: Int -> [[Int]] -> [[Int]]
prependOne value lists
    | null lists = []
    | otherwise = (value : head lists) : prependOne value (tail lists)

prependMany :: [Int] -> [[Int]] -> [[Int]]
prependMany values lists
    | null values = lists
    | otherwise = concatMap (`prependOne` lists) values

-- Generates rolls for the specified dice configuration.
-- [4, 6, 8, 8] would generate all the possible results for a d4, d6, d8, d8.
generateRolls :: [Int] -> [[Int]]
generateRolls configuration
    | null configuration = [[]]
    | otherwise = prependMany [1..(head configuration)] otherRolls
    where
        otherRolls = generateRolls (tail configuration)
