import Control.Applicative

main = do n <- read <$> getLine :: IO Int
          calcMany n

-- calcOne : process one line of input
calcOne :: IO ()
calcOne = do
    (_ : ts) <- words <$> getLine
    case eval' ts [] of
        False -> print 0
        True  -> print 1

-- calcMany n : process 'n' lines of input
calcMany :: Int -> IO ()
calcMany 1 = calcOne
calcMany n = do
    calcOne
    calcMany (n - 1)

-- eval src : the result of the evaluation of source 'src'
eval :: String -> Bool
eval src = eval' (words src) []

-- eval src bs : the result of the evaluation of source 'src'
--               using stack 'stack'
eval' :: [String] -> [Bool] -> Bool
eval' []     [b] = b
eval' (t:ts) bs
    | t == "0"   = eval' ts (False : bs)
    | t == "1"   = eval' ts (True  : bs)
    | t == "A"   = eval' ts (apply'and bs)
    | t == "R"   = eval' ts (apply'or  bs)
    | t == "X"   = eval' ts (apply'xor bs)
    | t == "N"   = eval' ts (apply'not bs)

-- apply'and bs : the stack obtained by applying an AND operator on stack 'bs'
apply'and :: [Bool] -> [Bool]
apply'and (b1:b2:bs) = (b1 && b2) : bs

-- apply'or bs : the stack obtained by applying an OR operator on stack 'bs'
apply'or :: [Bool] -> [Bool]
apply'or  (b1:b2:bs) = (b1 || b2) : bs

-- apply'xor bs : the stack obtained by applying an XOR operator on stack 'bs'
apply'xor :: [Bool] -> [Bool]
apply'xor (b1:b2:bs) = (b1 /= b2) : bs

-- apply'not bs : the stack obtained by applying a NOT operator on stack 'bs'
apply'not :: [Bool] -> [Bool]
apply'not (b:bs) = not b : bs
