main =
    getLine      >>=
    print . eval

-- eval src : the result of interpreting the program in 'src'
eval src = eval' (words src ) []

-- eval' tokens stack : the result of interpreting the token list 'tokens'
--                      using  the stack 'stack'
eval' :: [String] -> [Int] -> Int
eval' []       (n : _) = n
eval' (x : xs) stack   = case x of
                             "+" -> eval' xs (applyPlus stack)
                             "-" -> eval' xs (applyMinus stack)
                             "*" -> eval' xs (applyTimes stack)
                             str -> eval' xs (read str : stack)

-- apply'... : self-explanatory!
applyPlus, applyMinus, applyTimes :: [Int] -> [Int]
applyPlus  (n2 : n1 : ns) = n1 + n2 : ns
applyMinus (n2 : n1 : ns) = n1 - n2 : ns
applyTimes (n2 : n1 : ns) = n1 * n2 : ns
