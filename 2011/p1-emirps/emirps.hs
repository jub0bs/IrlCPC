import Control.Applicative  ( (<$>) )
import Data.List            ( intercalate )

main =
    readInputs >>= \(n, m) ->
    printList $ emirpRange n m

-- readInts : parse one line of input as a sequence of space-separated
--            integers
readInputs :: IO (Int, Int)
readInputs =
    map read . words <$> getLine >>= \[n, m] ->
    return (n, m)

-- printList ns : print the items of list 'xs' on a line, separated by spaces
printList :: Show a => [a] -> IO ()
printList ns =
    putStrLn $ intercalate " " $ map show ns

-- primes : the ascending list of prime numbers
primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

-- isPrime n : is positive integer 'n' prime?
isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = all (\p -> n `mod` p /= 0) primesPrefix
  where
    primesPrefix = takeWhile (\p -> p ^ 2 <= n) primes

-- emirpRange n m : the emirps between integers 'n' and 'm', inclusive
emirpRange :: Int -> Int -> [Int]
emirpRange n m = filter isEmirp $ takeWhile (<= m)
                                $ dropWhile (< n) primes

-- isEmirp p : is prime number 'p' an emirp?
isEmirp :: Int -> Bool
isEmirp p = isPrime revp && revp /= p
  where
    revp = (read . reverse . show) p
