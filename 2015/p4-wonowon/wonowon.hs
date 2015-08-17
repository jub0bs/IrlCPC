import Control.Applicative ((<$>))

main = do
    n <- read <$> getLine :: IO Int
    print $ wonowon n

-- primes : the ascending list of prime numbers
primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

-- isPrime n : is integer 'n' a prime number?
isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = all (\p -> n `mod` p /= 0) primesPrefix
  where
    primesPrefix = takeWhile (\p -> p ^ 2 <= n) primes

-- w p : W(p), the number of digits of the smallest wonowon number that is
--       divisible by prime number 'p'
w :: Int -> Int
w p = go 101 3
  where
    go n numdigits
        | r == 0    = numdigits
        | otherwise = go (100 * r + 1) $! numdigits + 2
      where
        r = n `mod` p

-- prop p : is W(p) equal to prime number 'p' minus 2?
prop :: Int -> Bool
prop p = w p == p - 2

-- wonowon n : the number of primes 'p' smaller than or equal to 'n' for
--             which  W(p) = p - 2
wonowon :: Int -> Int
wonowon n = length $ filter prop $ takeWhile (<= n) primes'
  where
    primes' = 3 : dropWhile (<= 5) primes
