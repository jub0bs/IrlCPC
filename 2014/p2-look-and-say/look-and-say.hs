main = do (iInt:n:[]) <- fmap (map read . words) getLine :: IO [Int]
          putStrLn $ lookAndSay n (show iInt)

-- lookAndSay n xs : the 'n'-th row of the look-and-say sequence,
--                   using string 'xs' as base case
lookAndSay :: Int -> String -> String
lookAndSay 1 = id
lookAndSay n = lookAndSay (n - 1) . lookAndSay1

-- lookAndSay xs : the next row of the look-and-say sequence,
--                 using string 'xs' as the current row
lookAndSay1 :: String -> String
lookAndSay1 xs = go xs (0,'0')
  where go []      (count, c) = show count ++ [c]
        go (c':cs) (0    , _) = go cs (1, c')
        go (c':cs) (count, c)
          | c' == c           = go cs (count + 1, c)
          | otherwise         = show count ++ c : go cs (1,c')
