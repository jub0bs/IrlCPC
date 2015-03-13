import Data.List (foldl')
import Data.Char (ord)

main = do xs <- getLine
          let (s1:s2:s3:[]) = words xs
          case solutions s1 s2 s3 of
            []    -> putStrLn "No solution"
            (n:_) -> print n

-- digits : the ascending list of characters that represent valid digits
digits :: [Char]
digits  = ['0' .. '9'] ++ ['A' .. 'Z']

-- candidateBases c : the list of bases (<= 32) in which character 'c'
--                    represents a valid digit
candidateBases :: Char -> [Int]
candidateBases c = [ v | (c', v) <- zip digits [1..32], c' >= c ]

-- solutions s1 s2 s3 : the list of bases in which the values of digit strings
--                      's1', 's2', and 's3' satisfy the equality
solutions :: String -> String -> String -> [Int]
solutions s1 s2 s3 = filter isSolution $ candidateBases maxDigit
  where isSolution b = fromBase b s1 + fromBase b s2 == fromBase b s3
        maxDigit     = maximum $ map maximum [s1,s2,s3]

-- fromBase b xs : the decimal value of the positive integer
--                 whose representation in base 'b' is string 'xs'
fromBase :: Int -> String -> Int
fromBase b = foldl' (\acc d -> b * acc + val d) 0

-- val c : the decimal value associated to character 'c'
val :: Char -> Int
val c
  | n >= 0 && n <= 9 = n
  | otherwise        = ord c - ord 'A' + 10
  where n = ord c - ord '0'
