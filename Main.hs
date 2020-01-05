module Main where
    import Commands
    import Interpreter
    import OutputGenerators
    import System.IO

    main = do
        putStr "Chose action:\n 1.Run a program\n 2.Concatenate two programs\n 3.Run two programs paralel\n 4.Alternate two programs\n"
        choice <- getLine
        case choice of
            "1" -> brainfuck
            "2" -> brainfuckConcat
            "3" -> brainfuckParalel
            "4" -> brainfuckAlternate
            _ -> putStr "YOU CAN ONLY TYPE 1,2,3 OR 4"

