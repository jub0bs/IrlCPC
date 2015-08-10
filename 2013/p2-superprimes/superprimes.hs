import Control.Applicative ( (<$>) )
import Data.List           ( intersperse )

main :: IO ()
main = do
    [a, b, n] <- map read . words <$> getLine :: IO [Int]
    putList $ superPrimes a b n

-- putList ns : print the list of integers
putList :: [Int] -> IO ()
putList = putStrLn . concat . intersperse " " . map show

-- primes : the ascending list of prime numbers
primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

-- isPrime n : is integer 'n' prime?
isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = all (\p -> n `mod` p /= 0) primesPrefix
  where
    primesPrefix = takeWhile (\p -> p ^ 2 <= n) primes

-- superPrimes a b n : the super prime list of order 'n', using the primes
--                     between 'a' and 'b' inclusive as base case
superPrimes :: Int -> Int -> Int -> [Int]
superPrimes a b n
    | n == 0 = takeWhile (<= b) $ dropWhile (< a) primes
    | otherwise = [ p | (i, p) <- zip [1 ..] (superPrimes a b (n - 1))
                      , isPrime i ]
