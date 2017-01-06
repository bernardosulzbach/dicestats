import Test.HUnit

import Generator
import DiceFilter

testGenerateRollsWithOneDice = TestCase (assertEqual "generateRolls [4]" [[1], [2], [3], [4]] (generateRolls [4]))
testGenerateRollsWithTwoDie = TestCase (assertEqual "generateRolls [2, 2]" [[1, 1], [1, 2], [2, 1], [2, 2]] (generateRolls [2, 2]))

testDropOneWithOneRoll = TestCase (assertEqual "dropOne [1]" [] (dropOne [1]))
testDropOneWithTwoRolls = TestCase (assertEqual "dropOne [1, 2]" [] (dropOne [2]))

tests = TestList [testGenerateRollsWithOneDice, testGenerateRollsWithTwoDie, testDropOneWithOneRoll, testDropOneWithTwoRolls]

main = runTestTT tests
