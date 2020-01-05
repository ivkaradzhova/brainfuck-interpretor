module OutputGenerators where
    import Interpreter
    import System.IO
    import Data.Char

    outputGenerator :: Program -> [Int]
    outputGenerator prog
        | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions prog))  = error "Unmatching brackets!"
        | (dp prog) < 0 = error "Trying to move the data pointer before the first element!"
        | (ip prog) >= length (instructions prog) = (output prog)
        | otherwise = outputGenerator  (readInstruction prog)


    addToOutput :: Int -> Program -> Program
    addToOutput ch p = Program {instructions=(instructions p), input=(input p), ip=(ip p),
                     dp=(dp p), dataTape=(dataTape p), output=((output p) ++ [ch])}

    paralelOutputGenerator :: Program -> Program -> [Int]
    paralelOutputGenerator p q 
        | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions p))  = error "Unmatching brackets in the first program!"
        | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions q))  = error "Unmatching brackets in the second program!"
        | (dp p) < 0 = error "Trying to move the data pointer before the first element in the first program!"
        | (dp q) < 0 = error "Trying to move the data pointer before the first element in the second program!"
        | (ip p) >= length (instructions p) && (ip q) >= length (instructions q) = if length (output p) > length (output q) then (output p) else (output q)
        | (ip p) >= length (instructions p) && (ip q) < length (instructions q) = paralelOutputGenerator p (readInstruction q)
        | (ip p) < length (instructions p) && (ip q) >= length (instructions q) = paralelOutputGenerator (readInstruction p) q
        | (instructions p) !! (ip p) == '.' = paralelOutputGenerator (readInstruction p) (addToOutput ((dataTape p) !! (dp p)) q)
        | (instructions q) !! (ip q) == '.' = paralelOutputGenerator (addToOutput ((dataTape q) !! (dp q)) p) (readInstruction q) 
        | otherwise = paralelOutputGenerator (readInstruction p) (readInstruction q)

    alternateOutputGenerator :: Program -> Program -> [Int]
    alternateOutputGenerator p q 
        | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions p))  = error "Unmatching brackets in the first program!"
        | not $ checkBrackets (filter (\x -> x == ']' || x == '[') (instructions q))  = error "Unmatching brackets in the second program!"
        | (dp p) < 0 = error "Trying to move the data pointer before the first element in the first program!"
        | (ip p) >= length (instructions p) = (output p)
        | (instructions p) !! (ip p) == '.' = alternateOutputGenerator (readInstruction (addToOutput ((dataTape p) !! (dp p)) q)) (readInstruction p)
        | otherwise = alternateOutputGenerator (readInstruction p) q
       