module DiceFilter where

import Data.List

dropOne :: [Int] -> [Int]
dropOne roll
    | null roll = roll
    | otherwise = delete (minimum roll) roll

-- Drops the specified die amount from each roll.
dropMany :: [Int] -> Int -> [Int]
dropMany roll amount
    | null roll || amount == 0 = roll
    | otherwise = dropMany (dropOne roll) (pred amount)
