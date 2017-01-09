import Test.HUnit

import Generator

testGenerateRollsWithOneDice = TestCase (assertEqual "generateRolls [4]" [[1], [2], [3], [4]] (generateRolls [4]))
testGenerateRollsWithTwoDie = TestCase (assertEqual "generateRolls [2, 2]" [[1, 1], [1, 2], [2, 1], [2, 2]] (generateRolls [2, 2]))

tests = TestList [testGenerateRollsWithOneDice, testGenerateRollsWithTwoDie]

main = runTestTT tests
