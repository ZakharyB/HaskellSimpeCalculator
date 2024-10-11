import Text.Read (readMaybe)

calculate :: Float -> String -> Float -> Float
calculate x op y = case op of
    "+" -> x + y
    "-" -> x - y
    "*" -> x * y
    "/" -> if y /= 0 then x / y else error "Division by zero"
    "^" -> x ** y
    "%" -> fromIntegral (round x `mod` round y)
    _ -> error "Invalid operator"

getFloat :: String -> IO Float
getFloat prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just num -> return num
        Nothing -> do
            putStrLn "Invalid input. Please enter a number"
            getFloat prompt

getOperator :: IO String
getOperator = do
    putStrLn "Enter operator (+, -, *, /, ^, %) or 'q' to quit:"
    op <- getLine
    if op `elem` ["+", "-", "*", "/", "^", "%", "q"]
        then return op
        else do
            putStrLn "Invalid operator. Please try again "
            getOperator

calculatorLoop :: IO ()
calculatorLoop = do
    x <- getFloat "Enter first number:"
    loop x
  where
    loop prevResult = do
        op <- getOperator
        if op == "q"
            then putStrLn $ "Fial result: " ++ show prevResult
            else do
                y <- getFloat "Enter next number:"
                let result = calculate prevResult op y
                putStrLn $ "Result: " ++ show result
                loop result

main :: IO ()
main = do
    putStrLn " Haskell Calculator"
    calculatorLoop