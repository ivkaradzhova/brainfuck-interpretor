
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- no incomplete patterns in lambdas!
{-# LANGUAGE InstanceSigs #-}                      -- allows us to write signatures in instance declarations


change :: Int -> [Int] -> (Int -> Int) -> [Int]
change 0 xs f = [f (xs !! 0)] ++ (tail xs)
change n xs f = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

indexAfter :: Char -> String -> Int
indexAfter ch [] = error ("No correspondin '" ++ [ch] ++ "' in the string!")
indexAfter ch (x:xs) = if x == ch then 1 else 1 + indexAfter ch xs

checkBrackets :: String -> Bool
checkBrackets xs = (not  (elem (-1) values)) &&  (last values == 0)
    where values = scanl (\acc x -> if x == '[' then 1 + acc else -1 + acc) 0 xs

--              instructions input     IP     DP    Data     output
readInstructions :: String -> [Int] -> Int -> Int -> [Int] -> [Int]
readInstructions instructions input ip dp dataTape
    | not $ checkBrackets (filter (\x -> x == ']' || x == '[') instructions)  = error "Unmatching brackets!"
    | dp < 0 = error "Trying to move the data pointer before the first element!"
    | ip >= length instructions = []
    | instructions !! ip == '>' = readInstructions instructions input (ip + 1) (dp + 1) dataTape
    | instructions !! ip == '<' = readInstructions instructions input (ip + 1) (dp - 1) dataTape
    | instructions !! ip == '+' = readInstructions instructions input (ip + 1)  dp (change dp dataTape succ)
    | instructions !! ip == '-' = readInstructions instructions input (ip + 1)  dp (change dp dataTape pred)
    | instructions !! ip == ',' && input /= [] 
                                = readInstructions instructions (tail input) (ip + 1)  dp (change dp dataTape (\x -> head input))
    | instructions !! ip == ',' && input == []
                                =  readInstructions instructions [] (ip + 1)  dp dataTape
    | instructions !! ip == '.' = (dataTape !! dp) : (readInstructions instructions input (ip + 1) dp dataTape)
    | instructions !! ip == '[' = if dataTape !! dp == 0
                                  then readInstructions instructions input (ip + (indexAfter ']' (drop ip instructions)))  dp dataTape
                                  else readInstructions instructions input (ip + 1) dp dataTape   
    | instructions !! ip == ']' = if dataTape !! dp == 0
                                  then readInstructions instructions input (ip + 1) dp dataTape       
                                  else readInstructions instructions input (ip - (indexAfter '[' (reverse (take ip instructions))))  dp dataTape     
    |otherwise = error ("Command '" ++ [instructions !! ip] ++ "' not defined!")



brainfuck :: String -> [Int] -> [Int]
brainfuck instructions input = readInstructions instructions input 0 0 [0,0..]
--main