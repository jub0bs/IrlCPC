import Control.Applicative ((<$>))
import Control.Monad       (replicateM_)

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          replicateM_ n calcOne

-- calcOne : process one line of input
calcOne :: IO ()
calcOne =
    words <$> getLine >>= \ (_ : ts) ->
    if eval' ts []
    then print 1
    else print 0

-- eval src : the result of the evaluation of source 'src'
eval :: String -> Bool
eval src = eval' (words src) []

-- eval src bs : the result of the evaluation of source 'src'
--               using Boolean stack 'bs'
eval' :: [String] -> [Bool] -> Bool
eval' []     [b] = b
eval' (t:ts) bs
    | t == "0" = eval' ts (False : bs)
    | t == "1" = eval' ts (True  : bs)
    | t == "A" = eval' ts (applyAnd bs)
    | t == "R" = eval' ts (applyOr  bs)
    | t == "X" = eval' ts (applyXor bs)
    | t == "N" = eval' ts (applyNot bs)

-- applyAnd bs : the stack obtained by applying an AND operator on the top
--               two elements of Boolean stack 'bs'
applyAnd :: [Bool] -> [Bool]
applyAnd (b1 : b2 : bs) = (b1 && b2) : bs

-- applyOr bs : the stack obtained by applying an OR operator on the top two
--              elements of Boolean stack 'bs'
applyOr :: [Bool] -> [Bool]
applyOr  (b1 : b2 : bs) = (b1 || b2) : bs

-- applyXor bs : the stack obtained by applying an XOR operator on the top
--               two elements of Boolean stack 'bs'
applyXor :: [Bool] -> [Bool]
applyXor (b1 : b2 : bs) = (b1 /= b2) : bs

-- applyNot bs : the stack obtained by applying a NOT operator on the top
--               element of Boolean stack 'bs'
applyNot :: [Bool] -> [Bool]
applyNot (b : bs) = not b : bs
