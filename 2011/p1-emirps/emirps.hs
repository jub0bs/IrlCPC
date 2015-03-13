main = do (n : m : []) <- readInputs
          putList $ emirpRange n m

-- readInts : parse one line of input as a sequence of space-separated integers
readInputs :: IO [Int]
readInputs = fmap (map read . words ) getLine

-- putList ns : print the list of integers 'ns'
putList :: [Int] -> IO ()
putList ns = case ns of []      -> return ()
                        [n]     -> putStrLn $ show n
                        (n:ns') -> do putStr $ show n
                                      putChar ' '
                                      putList ns'

-- primes : the ascending list of prime numbers
primes :: [Int]
primes = 2 : filter isPrime [3,5..]

-- isPrime n : is positive integer 'n' prime?
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\p -> n `mod` p /= 0) primesPrefix
  where primesPrefix = takeWhile (\p -> p^2 <= n) primes

-- emirpRange n m : the emirps between integers 'n' and 'm', inclusive
emirpRange :: Int -> Int -> [Int]
emirpRange n m = filter isEmirp $ takeWhile (<= m) $ dropWhile (< n) primes

-- isEmirp n : is prime number 'n' an emirp?
isEmirp :: Int -> Bool
isEmirp n = isPrime revn && revn /= n
  where revn = (read . reverse . show) n
