-- Based on Yiu-Kwong Man's article,
-- "An Arithmetic Approach to the General Two Water Jugs Problem"
--
import Data.Function ((&))
import Data.Functor ((<$>))
import Data.List (minimumBy, unlines)

main :: IO ()
main = do
    [m, n, d] <- map rd . words <$> getLine
    putStr $ unlines $ map show $ solve (m, n ,d)
    putStrLn "success"
  where
    rd = read :: String -> Int

data Jug = A | B deriving Show

data Action
  = Fill Jug
  | Pour Jug Jug
  | Empty Jug

instance Show Action where
  show (Fill jug) = "fill " ++ show jug
  show (Pour jugOut jugIn) = "pour " ++ show jugOut ++ " " ++ show jugIn
  show (Empty jug) = "empty " ++ show jug

solve :: (Int, Int, Int) -> [Action]
solve (m, n, d)
    | d == m = [Fill A]
    | d == n = [Fill B]
    | otherwise = minimumByLength solutions
  where
    solutions = map ($ (m, n, d)) [solve1, solve2]
    minimumByLength = minimumBy (\xs ys -> length xs `compare` length ys)

solve1 :: (Int, Int, Int) -> [Action]
solve1 (m, n, d) = actions $ reverse xs
  where
    (_, xs) = (0, []) & step2 & step3 & step4
    step2 (k, xs)
        | k /= d = go (k + m, m : xs)
        | otherwise = (k, xs)
      where
        go (k', ys)
            | k' == d || k' > n = (k', ys)
            | otherwise = go (k' + m, m : ys)
    step3 (k, xs)
        | k > n = (k - n, -n : xs)
        | otherwise = (k, xs)
    step4 (k, xs)
        | k == d = (k, xs)
        | otherwise = step2 (k, xs)
    actions = concat . map int2action
    int2action x
        | x == m = [Fill A, Pour A B]
        | x == -n = [Empty B, Pour A B]
        | otherwise = error "int2action: invalid Int value"

solve2 :: (Int, Int, Int) -> [Action]
solve2 (m, n, d) = actions $ reverse xs
  where
    (_, xs) = (0, []) & step2 & step3 & step4
    step2 (k, xs)
        | k /= d = (k + n, n : xs)
        | otherwise = (k, xs)
    step3 (k, xs)
        | k > d = go (k - m, -m : xs)
        | otherwise = (k, xs)
      where
        go (k', ys)
            | k' == d || k' < m = (k', ys)
            | otherwise = go (k' - m, -m : ys)
    step4 (k, xs)
        | k == d = (k, xs)
        | otherwise = step2 (k, xs)
    actions = concat . map int2action
    int2action x
        | x == n = [Fill B, Pour B A]
        | x == -m = [Empty A, Pour B A]
        | otherwise = error "int2action: invalid Int value"
