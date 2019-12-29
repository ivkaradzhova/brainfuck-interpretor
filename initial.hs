
import System.IO
import Data.Char

data Program = Program {
    instructions :: String,
    input :: [Int],
    ip :: Int,
    dp :: Int,
    dataTape :: [Int],
    output :: [Int]
} deriving (Show)

startingProgram :: String -> [Int] -> Program
startingProgram instr ip = Program {instructions = instr, input = ip, ip = 0,dp = 0, dataTape = [0,0..], output=[]}

change :: Int -> [Int] -> (Int -> Int) -> [Int]
change 0 xs f = [f (xs !! 0)] ++ (tail xs)
change n xs f = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

indexAfter :: Char -> String -> Int
indexAfter ch [] = error ("No correspondin '" ++ [ch] ++ "' in the string!")
indexAfter ch (x:xs) = if x == ch then 1 else 1 + indexAfter ch xs

checkBrackets :: String -> Bool
checkBrackets xs = (not  (elem (-1) values)) &&  (last values == 0)
    where values = scanl (\acc x -> if x == '[' then 1 + acc else -1 + acc) 0 xs


readInstruction :: Program -> Program
readInstruction prog 
    |instructions prog !! (ip prog) == '>' = 
        Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), 
                 dp=(dp prog + 1), dataTape=(dataTape prog), output=(output prog)}
    |instructions prog !! (ip prog) == '<' = 
        Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog),
                 dp=(dp prog - 1), dataTape=(dataTape prog), output=(output prog)}
    |instructions prog !! (ip prog) == '+' = 
        Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), 
                 dp=(dp prog), dataTape=(change (dp prog) (dataTape prog) succ), output=(output prog)}
    |instructions prog !! (ip prog) == '-' =
        Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog),
                 dp=(dp prog), dataTape=(change (dp prog) (dataTape prog) pred), output=(output prog)}
    |instructions prog !! (ip prog) == ',' && (input prog) /= [] =
        Program {instructions=(instructions prog), input=(tail (input prog)), ip=(1 + (ip prog)),
                 dp=(dp prog), dataTape=(change (dp prog) (dataTape prog) (\x -> (head $ input prog))), output=(output prog)}
    |instructions prog !! (ip prog) == ',' && (input prog) == [] =
        error ("All of the input has already been read!")
    |instructions prog !! (ip prog) == '.' = 
        Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog),
                 dp=(dp prog), dataTape=(dataTape prog), output=((output prog)++[(dataTape prog) !! (dp prog)])}
    |instructions prog !! (ip prog) == '[' = 
        if dataTape prog !! (dp prog) == 0
        then Program {instructions=(instructions prog), input=(input prog), ip=(ip prog + indexAfter ']' (drop (ip prog) (instructions prog))), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}
        else Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
    |instructions prog !! (ip prog) == ']' = 
        if (dataTape prog) !! (dp prog) == 0
        then Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
        else Program {instructions=(instructions prog), input=(input prog), ip=(ip prog - indexAfter '[' (reverse (take (ip prog) (instructions prog)))), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}
   |otherwise =
        Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
        
outputGenerator :: Program -> [Int]
outputGenerator prog
    | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions prog))  = error "Unmatching brackets!"
    | (dp prog) < 0 = error "Trying to move the data pointer before the first element!"
    | (ip prog) >= length (instructions prog) = (output prog)
    | otherwise = outputGenerator  (readInstruction prog)

brainfuck = do
    putStr "File Path: "
    fileName <- getLine
    contents <- readFile $ "/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ fileName
    putStr "Input: "
    input <- getLine
    putStr "Save output in: "
    outputFile <- getLine
    writeFile ("/home/pon/Documents/2.FunctionalProgramming/Brainfuck/TestFiles/" ++ outputFile)
                        $ map intToDigit $ outputGenerator $ startingProgram contents $ map digitToInt input
        

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
        $ map intToDigit $ outputGenerator $ startingProgram contents2 $ outputGenerator $ startingProgram contents1 $ map digitToInt input
       
addToOutput :: Int -> Program -> Program
addToOutput ch p = Program {instructions=(instructions p), input=(input p), ip=(ip p),
                 dp=(dp p), dataTape=(dataTape p), output=((output p) ++ [ch])}

paralelOutputGenerator :: Program -> Program -> [Int]
paralelOutputGenerator p q 
    | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions p))  = error "Unmatching brackets in the first program!"
    | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions q))  = error "Unmatching brackets in the second program!"
    | (dp p) < 0 = error "Trying to move the data pointer before the first element in the first program!"
    | (dp q) < 0 = error "Trying to move the data pointer before the first element in the second program!"
    | (instructions p) !! (ip p) == '.' = paralelOutputGenerator (readInstruction p) (addToOutput ((dataTape p) !! (dp p)) q)
    | (instructions q) !! (ip q) == '.' = paralelOutputGenerator (addToOutput ((dataTape q) !! (dp q)) p) (readInstruction q) 

    | (ip p) >= length (instructions p) && (ip q) >= length (instructions q) = (output p)
    | (ip p) >= length (instructions p) && (ip q) < length (instructions q) = paralelOutputGenerator p (readInstruction q)
    | (ip p) < length (instructions p) && (ip q) >= length (instructions q) = paralelOutputGenerator (readInstruction p) q
    | otherwise = paralelOutputGenerator (readInstruction p) (readInstruction q)
        

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
      $ map intToDigit $ paralelOutputGenerator (startingProgram contents1 (map digitToInt input)) (startingProgram contents2 (map digitToInt input))   


alternateOutputGenerator :: Program -> Program -> [Int]
alternateOutputGenerator p q 
    | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions p))  = error "Unmatching brackets in the first program!"
    | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions q))  = error "Unmatching brackets in the second program!"
    | (dp p) < 0 = error "Trying to move the data pointer before the first element in the first program!"
   -- | (dp q) < 0 = error "Trying to move the data pointer before the first element in the second program!"
    
    | (ip p) >= length (instructions p) && (ip q) >= length (instructions q) = (output p)
    | (ip p) >= length (instructions p) && (ip q) < length (instructions q) = alternateOutputGenerator (readInstruction q) p
   -- | (ip p) < length (instructions p) && (ip q) >= length (instructions q) = alternateOutputGenerator (readInstruction p) q
    | (instructions p) !! (ip p) == '.' = alternateOutputGenerator (readInstruction (addToOutput ((dataTape p) !! (dp p)) q)) p
    | otherwise = alternateOutputGenerator (readInstruction p) q
    

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
      $ map intToDigit $ alternateOutputGenerator (startingProgram contents1 (map digitToInt input)) (startingProgram contents2 (map digitToInt input))   

    


main = do
    putStr "Chose action:\n 1.Run a program\n 2.Concatenate two programs\n 3.Run two programs paralel\n 4.Alternate two programs\n"
    choice <- getLine
    case choice of
        "1" -> brainfuck
        "2" -> brainfuckConcat
        "3" -> brainfuckParalel
        "4" -> brainfuckAlternate
        _ -> putStr "Are you blind?! YOU CAN ONLY TYPE 1,2,3 OR 4! ISN'T IT CLEAR!!!!"

