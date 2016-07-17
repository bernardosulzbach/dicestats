import qualified Data.IntMap as IntMap
import System.Environment
import System.Exit
import System.IO
import Text.Printf

-- Statistical functions.

incrementValue :: IntMap.IntMap Int -> (Int, Int) -> Int -> IntMap.IntMap Int
incrementValue map node side = IntMap.insert newKey newValue map
    where
        newKey = (fst node) + side
        newValue = (snd node) + (IntMap.findWithDefault 0 newKey map)

addSideStep oldNodes newMap side
    | null oldNodes = newMap
    | otherwise = addSideStep (tail oldNodes) (incrementValue newMap (head oldNodes) side) side

-- The first roll of a 1d4 produces [(1, 1), (2, 1), (3, 1), (4, 1)]
-- When rolling another 1d4, we run four times (one for each side) over the old states and combine these to the new states.
-- After considering the 1 on the second dice, we get [(2, 1), (3, 1), (4, 1), (5, 1)].
-- After considering the 2 on the second dice, we get [(2, 1), (3, 2), (4, 2), (5, 2), (6, 1)], and so on.
addSide oldMap newMap side = addSideStep (IntMap.toList oldMap) newMap side

nextTableStep old new cur max
    | cur > max = new
    | otherwise = nextTableStep old (addSide old new cur) (succ cur) max

nextTable sides table = nextTableStep table IntMap.empty 1 sides

generateTableStep count sides table
    | count == 0 = table
    | otherwise = generateTableStep (pred count) sides (nextTable sides table)

generateTable :: Int -> Int -> IntMap.IntMap Int
generateTable count sides = generateTableStep count sides (IntMap.fromList [(0, 1)])

-- I/O functions.

splitOnIncrementalFinish cur strs
    | null cur = reverse strs
    | otherwise = reverse ((reverse cur) : strs)

splitOnIncremental :: String -> Char -> String -> [String] -> [String]
splitOnIncremental str sym cur strs
    | null str = splitOnIncrementalFinish cur strs
    | head str == sym = splitOnIncremental (tail str) sym [] ((reverse cur) : strs)
    | otherwise = splitOnIncremental (tail str) sym ((head str) : cur) strs

-- Splits the string at the specified character.
splitOn :: String -> Char -> [String]
splitOn str sym = splitOnIncremental str sym [] []

toInt str = read str :: Int

toDiceCount str
    | null str = 1
    | otherwise = toInt str

-- Sums all the counts in a table.
tableTotal table = IntMap.foldr (\ total current -> total + current) 0 table

digitCount positiveInteger
    | positiveInteger < 10 = 1
    | otherwise = 1 + (digitCount (quot positiveInteger 10)) 

rowToString :: (Int, Double) -> Int -> String
rowToString pair maxKey = printf ("%" ++ (show (digitCount (maxKey))) ++ "d: %.2f%%") (fst pair) (snd pair)

toPercentMap map = IntMap.map (\ count -> 100.0 * (fromIntegral count) / (fromIntegral (tableTotal map))) map

-- Pretty prints a table.
printTable table = putStr (unlines (fmap (\ pair -> rowToString pair (fst (head (IntMap.toDescList (toPercentMap table))))) (IntMap.toAscList (toPercentMap table))))

parseRollExpression exp
    | length split == 2 = printTable (generateTable (toDiceCount (head split)) (toInt (last split)))
    | otherwise = putStrLn "Unsupported roll expression."
    where
        split = splitOn exp 'd'

parseFullCalculation args = parseRollExpression (head args)

parseArguments args
    | null args || elem (head args) ["-h", "--help"] = printUsage >> exitWithSuccess
    | elem (head args) ["-v", "--version"] = printVersion >> exitWithSuccess
    | otherwise = parseFullCalculation args 

printVersion = putStrLn "Dice Statistics v1.0"
printUsage = putStrLn "Usage: dicestats xdy [<|>|=] z"

exitWithSuccess = exitWith ExitSuccess

main = getArgs >>= parseArguments
