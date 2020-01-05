module Interpreter(
    Program(..),
    startingProgram,
    checkBrackets,
    readInstruction
    ) where

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

    findCorrespondingOpeningBracketHelp :: Int -> Int -> [Int] -> String -> Int
    findCorrespondingOpeningBracketHelp end curr stack instr 
        | end == curr = last stack
        | (instr !! curr) == '[' = findCorrespondingOpeningBracketHelp end (curr + 1) (stack ++ [curr]) instr
        | (instr !! curr) == ']' = findCorrespondingOpeningBracketHelp end (curr + 1) (init stack) instr
        | otherwise = findCorrespondingOpeningBracketHelp end (curr + 1) stack instr

    findCorrespondingOpeningBracket :: Int -> String -> Int
    findCorrespondingOpeningBracket index instr = findCorrespondingOpeningBracketHelp index 0 [] instr       

    findCorrespondingClosingBracketHelp :: Int -> [Int] -> String -> Int
    findCorrespondingClosingBracketHelp curr stack instr 
        | (instr !! (curr - 1)) == ']' && (null stack) = curr - 1
        | (instr !! curr) == '[' = findCorrespondingClosingBracketHelp (curr + 1) (stack ++ [curr]) instr
        | (instr !! curr) == ']' = findCorrespondingClosingBracketHelp (curr + 1) (init stack) instr
        | otherwise = findCorrespondingClosingBracketHelp (curr + 1) stack instr

    findCorrespondingClosingBracket :: Int -> String -> Int
    findCorrespondingClosingBracket index instr = findCorrespondingClosingBracketHelp (index + 1) [index] instr       


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
                     dp=(-1 + dp prog), dataTape=(dataTape prog), output=(output prog)}
        |instructions prog !! (ip prog) == '+' = 
            Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), 
                     dp=(dp prog), dataTape=(change (dp prog) (dataTape prog) succ), output=(output prog)}
        |instructions prog !! (ip prog) == '-' =
            Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog),
                     dp=(dp prog), dataTape=(change (dp prog) (dataTape prog) pred), output=(output prog)}
        |instructions prog !! (ip prog) == ',' && (input prog) /= [] =
            Program {instructions=(instructions prog), input=(tail (input prog)), ip=(1 + ip prog),
                     dp=(dp prog), dataTape=(change (dp prog) (dataTape prog) (\x -> (head  (input prog)))), output=(output prog)}
        |instructions prog !! (ip prog) == ',' && (input prog) == [] =
            Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
        |instructions prog !! (ip prog) == '.' = 
            Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog),
                     dp=(dp prog), dataTape=(dataTape prog), output=((output prog)++[(dataTape prog) !! (dp prog)])}
        |instructions prog !! (ip prog) == '[' = 
            if dataTape prog !! (dp prog) <= 0
            then Program {instructions=(instructions prog), input=(input prog), ip=(1 + findCorrespondingClosingBracket (ip prog) (instructions prog)), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}
            else Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
        |instructions prog !! (ip prog) == ']' = 
            if (dataTape prog) !! (dp prog) <= 0
            then Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
            else Program {instructions=(instructions prog), input=(input prog), ip=(1 + findCorrespondingOpeningBracket (ip prog) (instructions prog)), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}
       |otherwise =
            Program {instructions=(instructions prog), input=(input prog), ip=(1 + ip prog), dp=(dp prog), dataTape=(dataTape prog), output=(output prog)}   
