import Data.Char           (digitToInt)
import Control.Applicative ((<$>))

main = do
    [xs, ys] <- words <$> getLine
    let n = read ys :: Int
    putStrLn $ lookAndSay n xs

-- lookAndSay n xs : the 'n'-th row of the look-and-say sequence,
--                   using string 'xs' as base case
lookAndSay :: Int -> String -> String
lookAndSay 1 = id
lookAndSay n = lookAndSay (n - 1) . lookAndSay1

-- lookAndSay1 xs : the next row of the look-and-say sequence,
--                  using string 'xs' as the current row
lookAndSay1 :: String -> String
lookAndSay1 xs = go xs (0, '0')
  where
    go []        (n, c)   = show n ++ [c]
    go (c' : cs) (0, '0') = go cs (1, c')
    go (c' : cs) (n, c)
        | c' == c         = go cs (n + 1, c')
        | otherwise       = show n ++ [c] ++ go cs (1,c')
