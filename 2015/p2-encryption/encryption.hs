import Control.Applicative
import Data.Char

main = do (n:a:b:[]) <- map read . words <$> getLine :: IO [Integer]
          xs         <- getLine
          putStrLn $ shiftStr (offset a b) xs

-- offset a b : the result of 'a' to the power 'b' modulo 26
offset :: Integer -> Integer -> Int
offset a b = negate $ fromIntegral $ (a' ^ b) `mod` 26
  where
    a' = a `mod` 26

-- shifChar o c : the character obtained by shifting character 'c' by 'n' places
--                (a space character is unaffected)
shiftChar :: Int -> Char -> Char
shiftChar o c
  | isSpace c   = c
  | otherwise   = chr $ ((ord c - ord 'A' + o) `mod` 26) + ord 'A'

-- shiftStr n cs : the string obtained by shifting each uppercase character
--                 in string 'cs' by 'n' places
shiftStr :: Int -> String -> String
shiftStr o xs = map (shiftChar $ fromIntegral o) xs
