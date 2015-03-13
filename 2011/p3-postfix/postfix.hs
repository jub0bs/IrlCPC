main = getLine >>=
       print . eval

-- eval src : the result of interpreting the program in 'src'
eval src = eval' (words src ) []

-- eval' tokens stack : the result of interpreting the token list 'tokens'
--                      using  the stack 'stack'
eval' :: [String] -> [Int] -> Int
eval' []     (n:_) = n
eval' (x:xs) stack = case x of
                       "+" -> eval' xs (apply'plus stack)
                       "-" -> eval' xs (apply'minus stack)
                       "*" -> eval' xs (apply'times stack)
                       str -> eval' xs (read str : stack)

-- apply'... : self-explanatory!
apply'plus, apply'minus, apply'times :: [Int] -> [Int]
apply'plus  (n2 : n1 : ns) = n1 + n2 : ns
apply'minus (n2 : n1 : ns) = n1 - n2 : ns
apply'times (n2 : n1 : ns) = n1 * n2 : ns
