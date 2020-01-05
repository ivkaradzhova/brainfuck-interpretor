module Commands where
    import Interpreter
    import OutputGenerators
    import System.IO
    import Data.Char
    
    intToString :: [Int] -> String
    intToString [] = []
    intToString (x:xs) = show x ++ " " ++ intToString xs 
          
    inputToListHelp :: [Int] -> String -> [Int]
    inputToListHelp curr [] = curr
    inputToListHelp curr (x:xs) = if generalCategory x == DecimalNumber 
        then if null curr 
            then inputToListHelp [(digitToInt x)] xs
            else inputToListHelp ((init curr) ++ [(last curr) * 10 + (digitToInt x)]) xs
        else inputToListHelp (curr ++ [0]) xs

    inputToList :: String -> [Int]
    inputToList = inputToListHelp []

    
    brainfuck = do
        putStr "File Path: "
        fileName <- getLine
        contents <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName
        putStr "Input: "
        input <- getLine
        putStr "Save output in: "
        outputFile <- getLine
        writeFile ("/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ outputFile)
                $ intToString $ outputGenerator $ startingProgram contents $ inputToList input
            

    brainfuckConcat = do
        putStr "First file path: "
        fileName1 <- getLine
        contents1 <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName1
        putStr "Second file path: "
        fileName2 <- getLine
        contents2 <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName2
        putStr "Input: "
        input <- getLine
        putStr "Save output in: "
        outputFile <- getLine
        writeFile ("/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ outputFile)
            $ intToString $ outputGenerator $ startingProgram contents2 $ outputGenerator $ startingProgram contents1 $ inputToList input

            

    brainfuckParalel = do
        putStr "First file path: "
        fileName1 <- getLine
        contents1 <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName1
        putStr "Second file path: "
        fileName2 <- getLine
        contents2 <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName2
        putStr "Input: "
        input <- getLine
        putStr "Save output in: "
        outputFile <- getLine
        writeFile ("/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ outputFile)
          $ intToString $ paralelOutputGenerator (startingProgram contents1 (inputToList input)) (startingProgram contents2 (inputToList input))   


        

    brainfuckAlternate = do
        putStr "First file path: "
        fileName1 <- getLine
        contents1 <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName1
        putStr "Second file path: "
        fileName2 <- getLine
        contents2 <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName2
        putStr "Input: "
        input <- getLine
        putStr "Save output in: "
        outputFile <- getLine
        writeFile ("/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ outputFile)
          $ intToString $ alternateOutputGenerator (startingProgram contents1 (inputToList input)) (startingProgram contents2 (inputToList input))   
    