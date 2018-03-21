------------------------------------------------------------------------------------------

module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.Process
import qualified Text.Printf as T

------------------------------------------------------------------------------------------

main = do
    
    args <- getArgs

    case args of
        [program]       -> runAllTests (programName program)
        [program, test] -> runSpecificTest (programName program) test
        _               -> putStrLn "Usage: runtests <program_to_test> [specific_test]"

------------------------------------------------------------------------------------------

programName :: String -> String
programName program
    | isSuffixOf ".hs" program = reverse . drop 3 . reverse $ program
    | otherwise                = program

compileProgram :: String -> IO String
compileProgram program = do

    tmpDir <- getTemporaryDirectory    
    time   <- getPOSIXTime

    let tmpExe = combine tmpDir (program ++ "_" ++ show (round time))

    readProcess
        "ghc"
        [
            program,
            "-main-is", program,
            "-fforce-recomp",
            "-o", tmpExe,
            "-outputdir", tmpDir
        ]
        ""

    return tmpExe

------------------------------------------------------------------------------------------

runAllTests :: String -> IO ()
runAllTests program = do

    putStrLn $ ">>> Running tests for \"" ++ program ++ "\" <<<"

    testFiles <- getDirectoryContents $ "../tests/" ++ program

    let inputTestFiles = filter (isSuffixOf ".in") testFiles
        testNames      = sort $ map (reverse . drop 3 . reverse) inputTestFiles

    executable <- compileProgram program

    mapM_ (runTest program executable) testNames

------------------------------------------------------------------------------------------

runTest :: String -> String -> String -> IO ()
runTest program executable testName = do

    let baseTestFilePath = "../tests/" ++ program ++ "/" ++ testName

    testInput      <- readFile $ baseTestFilePath ++ ".in"
    expectedOutput <- readFile $ baseTestFilePath ++ ".out"

    actualOutput <- readProcess executable [] testInput

    if actualOutput == expectedOutput
        then putStr "> Passed    - "
        else putStr "> FAILED :( - "

    putStrLn testName

------------------------------------------------------------------------------------------

runSpecificTest :: String -> String -> IO ()
runSpecificTest program testName = do

    putStrLn $ ">>> Running test \"" ++ testName ++ "\" for \"" ++ program ++ "\" <<<"

    let baseTestFilePath = "../tests/" ++ program ++ "/" ++ testName

    testInput      <- readFile $ baseTestFilePath ++ ".in"
    expectedOutput <- readFile $ baseTestFilePath ++ ".out"

    executable <- compileProgram program
    actualOutput <- readProcess executable [] testInput

    let numberedTestInput      = numberLines testInput
        numberedExpectedOutput = numberLines expectedOutput
        numberedActualOutput   = numberLines actualOutput

    if actualOutput == expectedOutput
        then do
            putStrLn "> Passed"
            putStrLn "> Input:"
            putStrLn numberedTestInput
            putStrLn "> Output:"
            putStrLn numberedActualOutput
            putStrLn "> END"
        else do
            putStrLn "> FAILED :("
            putStrLn "> Input:"
            putStrLn numberedTestInput
            putStrLn "> Expected output:"
            putStrLn numberedExpectedOutput
            putStrLn "> Actual output:"
            putStrLn numberedActualOutput
            putStrLn "> END"

------------------------------------------------------------------------------------------

numberLines :: String -> String
numberLines =
    intercalate "\n"
    . zipWith (\x y -> T.printf "%3d|" (x::Int) ++ "  " ++ y) [1..]
    . splitOn "\n"

------------------------------------------------------------------------------------------
