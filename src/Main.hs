import qualified Data.IntMap.Strict as IntMap
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Generator

-- Statistical functions.

incrementValue :: IntMap.IntMap Int -> (Int, Int) -> Int -> IntMap.IntMap Int
incrementValue map node side = IntMap.insert newKey newValue map
    where
        newKey = fst node + side
        newValue = snd node + IntMap.findWithDefault 0 newKey map

incrementCounter key table = IntMap.insert key (succ (IntMap.findWithDefault 0 key table)) table

addSideStep oldNodes newMap side
    | null oldNodes = newMap
    | otherwise = addSideStep (tail oldNodes) (incrementValue newMap (head oldNodes) side) side

-- The first roll of a 1d4 produces [(1, 1), (2, 1), (3, 1), (4, 1)]
-- When rolling another 1d4, we run four times (one for each side) over the old states and combine these to the new states.
-- After considering the 1 on the second dice, we get [(2, 1), (3, 1), (4, 1), (5, 1)].
-- After considering the 2 on the second dice, we get [(2, 1), (3, 2), (4, 2), (5, 2), (6, 1)], and so on.
addSide oldMap = addSideStep (IntMap.toList oldMap)

nextTableStep old new cur max
    | cur > max = new
    | otherwise = nextTableStep old (addSide old new cur) (succ cur) max

nextTable sides table = nextTableStep table IntMap.empty 1 sides

generateTableStep count sides table
    | count == 0 = table
    | otherwise = generateTableStep (pred count) sides (nextTable sides table)

generateTableFromRolls rolls partialTable
    | null rolls = partialTable
    | otherwise = generateTableFromRolls (tail rolls) (incrementCounter (sum (head rolls)) partialTable)

generateTable :: Int -> Int -> IntMap.IntMap Int
generateTable count sides = generateTableFromRolls (generateRolls (replicate count sides)) IntMap.empty

-- I/O functions.

splitOnIncrementalFinish cur strs
    | null cur = reverse strs
    | otherwise = reverse (reverse cur : strs)

splitOnIncremental :: String -> Char -> String -> [String] -> [String]
splitOnIncremental str sym cur strs
    | null str = splitOnIncrementalFinish cur strs
    | head str == sym = splitOnIncremental (tail str) sym [] (reverse cur : strs)
    | otherwise = splitOnIncremental (tail str) sym (head str : cur) strs

-- Splits the string at the specified character.
splitOn :: String -> Char -> [String]
splitOn str sym = splitOnIncremental str sym [] []

toInt str = read str :: Int

toDiceCount str
    | null str = 1
    | otherwise = toInt str

-- Sums all the counts in a table.
tableTotal = IntMap.foldr (+) 0

digitCount positiveInteger
    | positiveInteger < 10 = 1
    | otherwise = 1 + digitCount (quot positiveInteger 10)

rowToString :: Int -> (Int, Double) -> String
rowToString maxKey pair = printf ("%" ++ show (digitCount maxKey) ++ "d: %.2f%%") value probability
    where
        value = fst pair
        probability = snd pair

toPercentMap map = IntMap.map (\ count -> 100.0 * fromIntegral count / fromIntegral (tableTotal map)) map

-- Pretty prints a table.
printTable table = putStr $ unlines (fmap printRow ascendingList)
    where
        ascendingList = IntMap.toAscList (toPercentMap table)
        maximumKey = fst (head (IntMap.toDescList (toPercentMap table)))
        printRow = rowToString maximumKey

comparators = ["lt", "le", "eq", "ge", "gt"]
comparatorFunctions :: [Int -> Int -> Bool]
comparatorFunctions = [(<), (<=), (==), (>=), (>)]

parseRollExpression exp
    | length split == 2 = printTable (generateTable (toDiceCount (head split)) (toInt (last split)))
    | otherwise = putStrLn "Unsupported roll expression."
    where
        split = splitOn exp 'd'

addIfComparator :: (Int -> Bool) -> Int -> Double -> Double -> Double
addIfComparator function key element value
    | function key = value + element
    | otherwise = value

aggregate :: IntMap.IntMap Double -> String -> Int -> Double
aggregate table comparator value = IntMap.foldrWithKey (addIfComparator appliedComparatorFunction) 0.0 table
    where
        comparatorFunction = comparatorFunctions !! fromJust (elemIndex comparator comparators)
        appliedComparatorFunction = flip comparatorFunction value

parseRollExpressionWithValue :: [String] -> IO ()
parseRollExpressionWithValue arguments
    | comparator `notElem` comparators = printUnrecognizedComparator comparator >> exitFailure
    | length split == 2 = print (aggregate (toPercentMap table) comparator value)
    | otherwise = putStrLn "Unsupported roll expression."
    where
        exp = head arguments
        value = read (arguments !! 2) :: Int
        comparator = arguments !! 1
        split = splitOn exp 'd'
        table = generateTable (toDiceCount (head split)) (toInt (last split))

parseFullCalculation args
    | length args == 1 = parseRollExpression (head args)
    | length args == 3 = parseRollExpressionWithValue args
    | otherwise = printUsage >> exitSuccess

parseArguments args
    | null args || head args `elem` ["-h", "--help"] = printUsage >> exitSuccess
    | head args `elem` ["-v", "--version"] = printVersion >> exitSuccess
    | otherwise = parseFullCalculation args 

printVersion = putStrLn "Dice Statistics v1.0.0"
printUnrecognizedComparator comparator = putStrLn $ "Unrecognized comparator: " ++ comparator
printUsage = putStrLn $ "Usage: dicestats xdy [(" ++ intercalate "|" comparators ++ ") z]"

main = getArgs >>= parseArguments
