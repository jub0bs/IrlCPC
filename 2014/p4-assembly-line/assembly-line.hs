main = do n <- fmap read getLine :: IO Int
          ts <- getPairs n
          qs <- getQuads (n - 1)
          putIntList $ shortest $ dynprog ts qs

-- I/O boilerplate
getPairs :: Int -> IO [(Int,Int)]
getPairs 0 = return []
getPairs n = do (t1:t2:[]) <- fmap (map read . words) getLine :: IO [Int]
                ps <- getPairs (n - 1)
                return ((t1,t2):ps)

getQuads :: Int -> IO [(Int,Int,Int,Int)]
getQuads 0 = return []
getQuads n = do (m1:m2:c1:c2:[]) <- fmap (map read . words) getLine :: IO [Int]
                ps <- getQuads (n - 1)
                return ((m1,m2,c1,c2):ps)

putIntList :: [Int] -> IO ()
putIntList []     = return ()
putIntList [n]    = putStrLn $ show n
putIntList (n:ns) = do putStr $ show n
                       putChar ' '
                       putIntList ns

-- Some type and data declarations (for documentation purposes)
type Time         = Int
type StationTimes = (Time, Time)
type XTimes       = (Time, Time, Time, Time)
type Path         = [Int]  -- (not very type-safe, but time is of the essence)
data Schedule     = S Path -- the sequence of stations to the end
                      Time -- the time associated with the path

--  dynprog ts qs : the pair of optimal schedules starting from line 1 and 2
dynprog :: [StationTimes]
        -> [XTimes]
        -> (Schedule, Schedule)
dynprog [(t1, t2)]      []   = (S [1] t1, S [2] t2)
dynprog ((t1, t2) : ts) ((m1, m2, c1, c2) : qs)
  | not cross1 && not cross2 = (S (1 : p1) (t1 + m1'), S (2 : p2) (t2 + m2'))
  | not cross1 &&     cross2 = (S (1 : p1) (t1 + m1'), S (2 : p1) (t2 + c2'))
  |     cross1 && not cross2 = (S (1 : p2) (t1 + c1'), S (2 : p2) (t2 + m2'))
  |     cross1 &&     cross2 = (S (1 : p2) (t1 + c1'), S (2 : p1) (t2 + c2'))
  where m1'    = m1 + tau1
        m2'    = m2 + tau2
        c1'    = c1 + tau2
        c2'    = c2 + tau1
        cross1 = m1' > c1'
        cross2 = m2' > c2'
        (S p1 tau1, S p2 tau2) = dynprog ts qs

-- shortest (sched1, sched2) : the shortest path in the pair of schedules
shortest :: (Schedule, Schedule) -> Path
shortest (S p1 tau1, S p2 tau2)
    | tau1 < tau2 = p1
    | otherwise   = p2
